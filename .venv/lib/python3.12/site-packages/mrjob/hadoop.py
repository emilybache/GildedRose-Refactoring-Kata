# Copyright 2009-2016 Yelp and Contributors
# Copyright 2017 Yelp
# Copyright 2018 Yelp and Google, Inc.
# Copyright 2019 Yelp
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
import getpass
import logging
import os
import posixpath
import re
from subprocess import CalledProcessError
from subprocess import Popen
from subprocess import PIPE

try:
    import pty
    pty  # quiet "redefinition of unused ..." warning from pyflakes
except ImportError:
    pty = None

from mrjob.bin import MRJobBinRunner
from mrjob.compat import uses_yarn
from mrjob.conf import combine_dicts
from mrjob.fs.composite import CompositeFilesystem
from mrjob.fs.hadoop import HadoopFilesystem
from mrjob.fs.local import LocalFilesystem
from mrjob.logs.counters import _pick_counters
from mrjob.logs.errors import _log_probable_cause_of_failure
from mrjob.logs.mixin import LogInterpretationMixin
from mrjob.logs.step import _eio_to_eof
from mrjob.logs.step import _interpret_hadoop_jar_command_stderr
from mrjob.logs.step import _is_counter_log4j_record
from mrjob.logs.step import _log_line_from_driver
from mrjob.logs.step import _log_log4j_record
from mrjob.logs.wrap import _logs_exist
from mrjob.parse import is_uri
from mrjob.py2 import to_unicode
from mrjob.runner import _fix_env
from mrjob.setup import UploadDirManager
from mrjob.step import StepFailedException
from mrjob.step import _is_spark_step_type
from mrjob.util import cmd_line
from mrjob.util import unique
from mrjob.util import which


log = logging.getLogger(__name__)

# don't look for the hadoop streaming jar here!
_BAD_HADOOP_HOMES = ['/', '/usr', '/usr/local']

# where YARN stores history logs, etc. on HDFS by default
_DEFAULT_YARN_HDFS_LOG_DIR = 'hdfs:///tmp/hadoop-yarn/staging'

# places to look for the Hadoop streaming jar if we're inside EMR
_EMR_HADOOP_STREAMING_JAR_DIRS = [
    # for the 2.x and 3.x AMIs (the 2.x AMIs also set $HADOOP_HOME properly)
    '/home/hadoop/contrib',
    # for the 4.x AMIs
    '/usr/lib/hadoop-mapreduce',
]

# these are fairly standard places to keep Hadoop logs
_FALLBACK_HADOOP_LOG_DIRS = [
    '/var/log/hadoop',
    '/mnt/var/log/hadoop',  # EMR's 2.x and 3.x AMIs use this
]

# fairly standard places to keep YARN logs (see #1339)
_FALLBACK_HADOOP_YARN_LOG_DIRS = [
    '/var/log/hadoop-yarn',
    '/mnt/var/log/hadoop-yarn',
]

# start of Counters printed by Hadoop
_HADOOP_COUNTERS_START_RE = re.compile(br'^Counters: (?P<amount>\d+)\s*$')

# header for a group of counters
_HADOOP_COUNTER_GROUP_RE = re.compile(br'^(?P<indent>\s+)(?P<group>.*)$')

# line for a counter
_HADOOP_COUNTER_RE = re.compile(
    br'^(?P<indent>\s+)(?P<counter>.*)=(?P<amount>\d+)\s*$')

# the one thing Hadoop streaming prints to stderr not in log format
_HADOOP_NON_LOG_LINE_RE = re.compile(r'^Streaming Command Failed!')

# if we see this from Hadoop, it actually came from stdout and shouldn't
# be logged
_HADOOP_STDOUT_RE = re.compile(br'^packageJobJar: ')

# match the filename of a hadoop streaming jar
_HADOOP_STREAMING_JAR_RE = re.compile(
    r'^hadoop.*streaming.*(?<!-sources)\.jar$')


def fully_qualify_hdfs_path(path):
    """If path isn't an ``hdfs://`` URL, turn it into one."""
    if is_uri(path):
        return path
    elif path.startswith('/'):
        return 'hdfs://' + path
    else:
        return 'hdfs:///user/%s/%s' % (getpass.getuser(), path)


class HadoopJobRunner(MRJobBinRunner, LogInterpretationMixin):
    """Runs an :py:class:`~mrjob.job.MRJob` on your Hadoop cluster.
    Invoked when you run your job with ``-r hadoop``.

    Input and support files can be either local or on HDFS; use ``hdfs://...``
    URLs to refer to files on HDFS.
    """
    alias = 'hadoop'

    OPT_NAMES = MRJobBinRunner.OPT_NAMES | {
        'hadoop_bin',
        'hadoop_extra_args',
        'hadoop_log_dirs',
        'hadoop_streaming_jar',
        'hadoop_tmp_dir',
        'spark_deploy_mode',
        'spark_master',
    }

    # supports everything (so far)
    _STEP_TYPES = {
        'jar', 'spark', 'spark_jar', 'spark_script', 'streaming'}

    def __init__(self, **kwargs):
        """:py:class:`~mrjob.hadoop.HadoopJobRunner` takes the same arguments
        as :py:class:`~mrjob.runner.MRJobRunner`, plus some additional options
        which can be defaulted in :ref:`mrjob.conf <mrjob.conf>`.
        """
        super(HadoopJobRunner, self).__init__(**kwargs)

        self._hadoop_tmp_dir = fully_qualify_hdfs_path(
            posixpath.join(
                self._opts['hadoop_tmp_dir'], self._job_key))

        # Keep track of local files to upload to HDFS. We'll add them
        # to this manager just before we need them.
        hdfs_files_dir = posixpath.join(self._hadoop_tmp_dir, 'files', '')
        self._upload_mgr = UploadDirManager(hdfs_files_dir)

        # Set output dir if it wasn't set explicitly
        self._output_dir = fully_qualify_hdfs_path(
            self._output_dir or
            posixpath.join(self._hadoop_tmp_dir, 'output'))

        # Fully qualify step_output_dir, if set
        if self._step_output_dir:
            self._step_output_dir = fully_qualify_hdfs_path(
                self._step_output_dir)

        # Track job and (YARN) application ID to enable log parsing
        self._application_id = None
        self._job_id = None

        # Keep track of where the hadoop streaming jar is
        self._hadoop_streaming_jar = self._opts['hadoop_streaming_jar']
        self._searched_for_hadoop_streaming_jar = False

        # List of dicts (one for each step) potentially containing
        # the keys 'history', 'step', and 'task' ('step' will always
        # be filled because it comes from the hadoop jar command output,
        # others will be filled as needed)
        self._log_interpretations = []

    @classmethod
    def _default_opts(cls):
        return combine_dicts(
            super(HadoopJobRunner, cls)._default_opts(),
            dict(
                hadoop_tmp_dir='tmp/mrjob',
            )
        )

    @property
    def fs(self):
        """:py:class:`mrjob.fs.base.Filesystem` object for HDFS and the local
        filesystem.
        """
        if self._fs is None:
            self._fs = CompositeFilesystem()

            # don't pass [] to fs; this means not to use hadoop until
            # fs.set_hadoop_bin() is called (used for running hadoop over SSH).
            hadoop_bin = self._opts['hadoop_bin'] or None

            self._fs.add_fs('hadoop',
                            HadoopFilesystem(hadoop_bin))
            self._fs.add_fs('local', LocalFilesystem())

        return self._fs

    def get_hadoop_version(self):
        """Invoke the hadoop executable to determine its version"""
        return self.fs.hadoop.get_hadoop_version()

    def get_hadoop_bin(self):
        """Find the hadoop binary. A list: binary followed by arguments."""
        return self.fs.hadoop.get_hadoop_bin()

    def get_hadoop_streaming_jar(self):
        """Find the path of the hadoop streaming jar, or None if not found."""
        if not (self._hadoop_streaming_jar or
                self._searched_for_hadoop_streaming_jar):

            self._hadoop_streaming_jar = self._find_hadoop_streaming_jar()

            if self._hadoop_streaming_jar:
                log.info('Found Hadoop streaming jar: %s' %
                         self._hadoop_streaming_jar)
            else:
                log.warning('Hadoop streaming jar not found. Use'
                            ' --hadoop-streaming-jar')

            self._searched_for_hadoop_streaming_jar = True

        return self._hadoop_streaming_jar

    def _find_hadoop_streaming_jar(self):
        """Search for the hadoop streaming jar. See
        :py:meth:`_hadoop_streaming_jar_dirs` for where we search."""
        for path in unique(self._hadoop_streaming_jar_dirs()):
            log.info('Looking for Hadoop streaming jar in %s...' % path)

            streaming_jars = []
            for path in self.fs.ls(path):
                if _HADOOP_STREAMING_JAR_RE.match(posixpath.basename(path)):
                    streaming_jars.append(path)

            if streaming_jars:
                # prefer shorter names and shallower paths
                def sort_key(p):
                    return (len(p.split('/')),
                            len(posixpath.basename(p)),
                            p)

                streaming_jars.sort(key=sort_key)

                return streaming_jars[0]

        return None

    def _hadoop_dirs(self):
        """Yield all possible hadoop directories (used for streaming jar
        and logs). May yield duplicates"""
        for name in ('HADOOP_PREFIX', 'HADOOP_HOME', 'HADOOP_INSTALL',
                     'HADOOP_MAPRED_HOME'):
            path = os.environ.get(name)
            if path:
                yield path

        # guess it from the path of the Hadoop binary
        hadoop_home = _hadoop_prefix_from_bin(self.get_hadoop_bin()[0])
        if hadoop_home:
            yield hadoop_home

        # try HADOOP_*_HOME
        for name, path in sorted(os.environ.items()):
            if name.startswith('HADOOP_') and name.endswith('_HOME'):
                yield path

    def _hadoop_streaming_jar_dirs(self):
        """Yield all possible places to look for the Hadoop streaming jar.
        May yield duplicates.
        """
        for hadoop_dir in self._hadoop_dirs():
            yield hadoop_dir

        # use hard-coded paths to work out-of-the-box on EMR
        for path in _EMR_HADOOP_STREAMING_JAR_DIRS:
            yield path

    def _hadoop_log_dirs(self, output_dir=None):
        """Yield all possible places to look for hadoop logs."""
        # hadoop_log_dirs opt overrides all this
        if self._opts['hadoop_log_dirs']:
            for path in self._opts['hadoop_log_dirs']:
                yield path
            return

        hadoop_log_dir = os.environ.get('HADOOP_LOG_DIR')
        if hadoop_log_dir:
            yield hadoop_log_dir

        yarn = uses_yarn(self.get_hadoop_version())

        if yarn:
            yarn_log_dir = os.environ.get('YARN_LOG_DIR')
            if yarn_log_dir:
                yield yarn_log_dir

            yield _DEFAULT_YARN_HDFS_LOG_DIR

        if output_dir:
            # Cloudera style of logging
            yield posixpath.join(output_dir, '_logs')

        for hadoop_dir in self._hadoop_dirs():
            yield posixpath.join(hadoop_dir, 'logs')

        # hard-coded fallback paths
        if yarn:
            for path in _FALLBACK_HADOOP_YARN_LOG_DIRS:
                yield path

        for path in _FALLBACK_HADOOP_LOG_DIRS:
            yield path

    def _run(self):
        self._find_binaries_and_jars()
        self._create_setup_wrapper_scripts()
        self._add_job_files_for_upload()
        self._upload_local_files()
        self._run_job_in_hadoop()

    def _find_binaries_and_jars(self):
        """Find hadoop and (if needed) spark-submit bin up-front, before
        continuing with the job.

        (This is just for user-interaction purposes; these would otherwise
        lazy-load as needed.)
        """
        # this triggers looking for Hadoop binary
        self.get_hadoop_version()

        if self._has_hadoop_streaming_steps():
            self.get_hadoop_streaming_jar()

        if self._has_spark_steps():
            self.get_spark_submit_bin()

    def _add_job_files_for_upload(self):
        """Add files needed for running the job (setup and input)
        to self._upload_mgr."""
        for path in self._py_files():
            self._upload_mgr.add(path)

    def _dump_stdin_to_local_file(self):
        """Dump sys.stdin to a local file, and return the path to it."""
        stdin_path = posixpath.join(self._get_local_tmp_dir(), 'STDIN')
         # prompt user, so they don't think the process has stalled
        log.info('reading from STDIN')

        log.debug('dumping stdin to local file %s...' % stdin_path)
        stdin_file = open(stdin_path, 'wb')
        for line in self._stdin:
            stdin_file.write(line)

        return stdin_path

    def _run_job_in_hadoop(self):
        for step_num, step in enumerate(self._get_steps()):
            step_type = step['type']
            step_args = self._args_for_step(step_num)
            env = _fix_env(self._env_for_step(step_num))

            # log this *after* _args_for_step(), which can start a search
            # for the Hadoop streaming jar
            log.info('Running step %d of %d...' %
                     (step_num + 1, self._num_steps()))
            log.debug('> %s' % cmd_line(step_args))
            log.debug('  with environment: %r' % sorted(env.items()))

            log_interpretation = {}
            self._log_interpretations.append(log_interpretation)

            if self._step_type_uses_spark(step_type):
                returncode, step_interpretation = self._run_spark_submit(
                    step_args, env, record_callback=_log_log4j_record)
            else:
                returncode, step_interpretation = self._run_hadoop(
                    step_args, env, record_callback=_log_record_from_hadoop)

            # make sure output_dir is filled (used for history log)
            if 'output_dir' not in step_interpretation:
                step_interpretation['output_dir'] = (
                    self._step_output_uri(step_num))

            log_interpretation['step'] = step_interpretation

            self._log_counters(log_interpretation, step_num)

            if returncode:
                error = self._pick_error(log_interpretation, step_type)
                if error:
                    _log_probable_cause_of_failure(log, error)

                # use CalledProcessError's well-known message format
                reason = str(CalledProcessError(returncode, step_args))
                raise StepFailedException(
                    reason=reason, step_num=step_num,
                    num_steps=self._num_steps())

    def _run_hadoop(self, hadoop_args, env, record_callback):
        # try to use a PTY if it's available
        try:
            pid, master_fd = pty.fork()
        except (AttributeError, OSError):
            # no PTYs, just use Popen

            # user won't get much feedback for a while, so tell them
            # Hadoop is running
            log.debug('No PTY available, using Popen() to invoke Hadoop')

            step_proc = Popen(hadoop_args, stdout=PIPE, stderr=PIPE, env=env)

            step_interpretation = _interpret_hadoop_jar_command_stderr(
                step_proc.stderr,
                record_callback=_log_record_from_hadoop)

            # there shouldn't be much output to STDOUT
            for line in step_proc.stdout:
                _log_line_from_driver(to_unicode(line).strip('\r\n'))

            step_proc.stdout.close()
            step_proc.stderr.close()

            returncode = step_proc.wait()
        else:
            # we have PTYs
            if pid == 0:  # we are the child process
                try:
                    os.execvpe(hadoop_args[0], hadoop_args, env)
                    # now we are no longer Python
                except OSError as ex:
                    # use _exit() so we don't do cleanup, etc. that's
                    # the parent process's job
                    os._exit(ex.errno)
                finally:
                    # if we got some other exception, still exit hard
                    os._exit(-1)
            else:
                log.debug('Invoking Hadoop via PTY')

                with os.fdopen(master_fd, 'rb') as master:
                    # reading from master gives us the subprocess's
                    # stderr and stdout (it's a fake terminal)
                    step_interpretation = (
                        _interpret_hadoop_jar_command_stderr(
                            _eio_to_eof(master),
                            record_callback=_log_record_from_hadoop))
                    _, returncode = os.waitpid(pid, 0)

        return returncode, step_interpretation

    def _spark_master(self):
        return self._opts['spark_master'] or 'yarn'

    def _args_for_step(self, step_num):
        step = self._get_step(step_num)

        if step['type'] == 'streaming':
            return self._args_for_streaming_step(step_num)
        elif step['type'] == 'jar':
            return self._args_for_jar_step(step_num)
        elif _is_spark_step_type(step['type']):
            return self._args_for_spark_step(step_num)
        else:
            raise ValueError('Bad step type: %r' % (step['type'],))

    def _args_for_streaming_step(self, step_num):
        hadoop_streaming_jar = self.get_hadoop_streaming_jar()
        if not hadoop_streaming_jar:
            raise Exception('no Hadoop streaming jar')

        return (self.get_hadoop_bin() + ['jar', hadoop_streaming_jar] +
                self._hadoop_streaming_jar_args(step_num))

    def _args_for_jar_step(self, step_num):
        step = self._get_step(step_num)

        args = []

        args.extend(self.get_hadoop_bin())

        # special case for consistency with EMR runner.
        #
        # This might look less like duplicated code if we ever
        # implement #780 (fetching jars from URIs)
        if step['jar'].startswith('file:///'):
            jar = step['jar'][7:]  # keep leading slash
        else:
            jar = step['jar']

        args.extend(['jar', jar])

        if step.get('main_class'):
            args.append(step['main_class'])

        if step.get('args'):
            args.extend(
                self._interpolate_jar_step_args(step['args'], step_num))

        return args

    def _env_for_step(self, step_num):
        step = self._get_step(step_num)

        env = dict(os.environ)

        # when running spark-submit, set its environment directly. See #1464
        if _is_spark_step_type(step['type']):
            env.update(self._spark_cmdenv(step_num))

        return env

    def _default_step_output_dir(self):
        return posixpath.join(self._hadoop_tmp_dir, 'step-output')

    def _cleanup_hadoop_tmp(self):
        if self._hadoop_tmp_dir:
            log.info('Removing HDFS temp directory %s...' %
                     self._hadoop_tmp_dir)
            try:
                self.fs.rm(self._hadoop_tmp_dir)
            except Exception as e:
                log.exception(e)

    def _manifest_download_commands(self):
        cp_to_local = self.get_hadoop_bin() + ['fs', '-copyToLocal']

        return [
            ('*://*', cmd_line(cp_to_local)),
        ]

    ### LOG (implementation of LogInterpretationMixin) ###

    def _stream_history_log_dirs(self, output_dir=None):
        """Yield lists of directories to look for the history log in."""
        if not self._read_logs():
            return

        for log_dir in unique(self._hadoop_log_dirs(output_dir=output_dir)):
            if _logs_exist(self.fs, log_dir):
                log.info('Looking for history log in %s...' % log_dir)
                # logs aren't always in a subdir named history/
                yield [log_dir]

    def _stream_task_log_dirs(self, application_id=None, output_dir=None):
        """Yield lists of directories to look for the task logs in."""
        # Note: this is unlikely to be super-helpful on "real" (multi-node)
        # pre-YARN Hadoop because task logs aren't generally shipped to a
        # local directory. It's a start, anyways. See #1201.
        if not self._read_logs():
            return

        for log_dir in unique(self._hadoop_log_dirs(output_dir=output_dir)):
            if application_id:
                path = self.fs.join(log_dir, 'userlogs', application_id)
            else:
                path = self.fs.join(log_dir, 'userlogs')

            if _logs_exist(self.fs, path):
                log.info('Looking for task syslogs in %s...' % path)
                yield [path]

    def counters(self):
        return [_pick_counters(log_interpretation)
                for log_interpretation in self._log_interpretations]


# These don't require state from HadoopJobRunner, so making them functions.
# Feel free to convert them back into methods as need be


def _hadoop_prefix_from_bin(hadoop_bin):
    """Given a path to the hadoop binary, return the path of the implied
    hadoop home, or None if we don't know.

    Don't return the parent directory of directories in the default
    path (not ``/``, ``/usr``, or ``/usr/local``).
    """
    # resolve unqualified binary name (relative paths are okay)
    if '/' not in hadoop_bin:
        hadoop_bin = which(hadoop_bin)
        if not hadoop_bin:
            return None

    # use parent of hadoop_bin's directory
    hadoop_home = posixpath.abspath(
        posixpath.join(posixpath.realpath(posixpath.dirname(hadoop_bin)), '..')
    )

    if hadoop_home in _BAD_HADOOP_HOMES:
        return None

    return hadoop_home


def _log_record_from_hadoop(record):
    """Log log4j record parsed from hadoop stderr."""
    if not _is_counter_log4j_record(record):  # counters are printed separately
        _log_log4j_record(record)
