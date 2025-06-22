# -*- coding: utf-8 -*-
# Copyright 2009-2017 Yelp and Contributors
# Copyright 2018-2019 Yelp
# Copyright 2020 Affirm, Inc. and Contributors
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
"""Abstract base class for all runners that execute binaries/scripts
(that is, everything but inline mode).
"""
import logging
import os
import os.path
import pipes
import re
import sys
from mrjob.py2 import PY2
from platform import python_implementation
from subprocess import Popen
from subprocess import PIPE

try:
    import pty
    pty  # quiet "redefinition of unused ..." warning from pyflakes
except ImportError:
    pty = None

try:
    import pyspark
    pyspark  # quiet "redefinition of unused ..." warning from pyflakes
except ImportError:
    pyspark = None

import mrjob.step
from mrjob.compat import translate_jobconf
from mrjob.conf import combine_cmds
from mrjob.conf import combine_dicts
from mrjob.logs.log4j import _parse_hadoop_log4j_records
from mrjob.logs.spark import _parse_spark_log
from mrjob.logs.step import _eio_to_eof
from mrjob.py2 import string_types
from mrjob.runner import MRJobRunner
from mrjob.setup import parse_setup_cmd
from mrjob.util import cmd_line
from mrjob.util import shlex_split
from mrjob.util import unique
from mrjob.util import which
from mrjob.util import zip_dir

log = logging.getLogger(__name__)

# no need to escape arguments that only include these characters
_HADOOP_SAFE_ARG_RE = re.compile(r'^[\w\./=-]*$')

# used to handle manifest files
_MANIFEST_INPUT_FORMAT = 'org.apache.hadoop.mapred.lib.NLineInputFormat'

# map archive file extensions to the command used to unarchive them
_EXT_TO_UNARCHIVE_CMD = {
    '.zip': 'unzip -o %(file)s -d %(dir)s',
    '.tar': 'mkdir %(dir)s; tar xf %(file)s -C %(dir)s',
    '.tar.gz': 'mkdir %(dir)s; tar xfz %(file)s -C %(dir)s',
    '.tgz': 'mkdir %(dir)s; tar xfz %(file)s -C %(dir)s',
}


def _unarchive_cmd(path):
    """Look up the unarchive command to use with the given file extension,
    or raise KeyError if there is no matching command."""
    for ext, unarchive_cmd in sorted(_EXT_TO_UNARCHIVE_CMD.items()):
        # use this so we can match e.g. mrjob-0.7.0.tar.gz
        if path.endswith(ext):
            return unarchive_cmd

    raise KeyError('unknown archive type: %s' % path)


class MRJobBinRunner(MRJobRunner):

    OPT_NAMES = MRJobRunner.OPT_NAMES | {
        'python_bin',
        'sh_bin',
        'spark_args',
        'spark_submit_bin',
        'task_python_bin',
    }

    def __init__(self, **kwargs):
        super(MRJobBinRunner, self).__init__(**kwargs)

        # where a zip file of the mrjob library is stored locally
        self._mrjob_zip_path = None

        # we'll create the setup wrapper scripts later
        self._setup_wrapper_script_path = None
        self._manifest_setup_script_path = None
        self._spark_python_wrapper_path = None

        # self._setup is a list of shell commands with path dicts
        # interleaved; see mrjob.setup.parse_setup_cmd() for details
        self._setup = [parse_setup_cmd(cmd) for cmd in self._opts['setup']]

        for cmd in self._setup:
            for token in cmd:
                if isinstance(token, dict):
                    # convert dir archives tokens to archives
                    if token['type'] == 'dir':
                        # feed the archive's path to self._working_dir_mgr
                        token['path'] = self._dir_archive_path(token['path'])
                        token['type'] = 'archive'

                    self._working_dir_mgr.add(**token)

        # warning: no setup scripts on Spark when no working dir
        if self._setup and self._has_pyspark_steps() and not(
                self._spark_executors_have_own_wd()):
            log.warning("setup commands aren't supported on Spark master %r" %
                        self._spark_master())

        # --py-files on Spark doesn't allow '#' (see #1375)
        if any('#' in path for path in self._opts['py_files']):
            raise ValueError("py_files cannot contain '#'")

        # Keep track of where the spark-submit binary is
        self._spark_submit_bin = self._opts['spark_submit_bin']

    @classmethod
    def _default_opts(cls):
        return combine_dicts(
            super(MRJobBinRunner, cls)._default_opts(),
            dict(
                read_logs=True,
            )
        )

    def _fix_opt(self, opt_key, opt_value, source):
        """Check sh_bin"""
        opt_value = super(MRJobBinRunner, self)._fix_opt(
            opt_key, opt_value, source)

        # check that sh_bin doesn't have too many args
        if opt_key == 'sh_bin':
            # opt_value is usually a string, combiner makes it a list of args
            sh_bin = combine_cmds(opt_value)

            # empty sh_bin just means to use the default, see #1926

            # make these hard requirements in v0.7.0?
            if len(sh_bin) > 1 and not os.path.isabs(sh_bin[0]):
                log.warning('sh_bin (from %s) should use an absolute path'
                            ' if you want it to take arguments' % source)
            elif len(sh_bin) > 2:
                log.warning('sh_bin (from %s) should not take more than one'
                            ' argument' % source)

        return opt_value

    ### python binary ###

    def _python_bin(self):
        """Python binary used for everything other than invoking the job.

        For running job tasks (e.g. ``--mapper``, ``--spark``), we use
        :py:meth:`_task_python_bin`, which can be set to a different value
        by setting :mrjob-opt:`task_python_bin`.

        Ways mrjob uses Python other than running tasks:
         * file locking in setup wrapper scripts
         * finding site-packages dir to bootstrap mrjob on clusters
         * invoking ``cat.py`` in local mode
         * the Python binary for Spark (``$PYSPARK_PYTHON``)
        """
        # python_bin isn't an option for inline runners
        return self._opts['python_bin'] or self._default_python_bin()

    def _task_python_bin(self):
        """Python binary used to invoke job with ``--mapper``,
        ``--reducer``, ``--spark``, etc."""
        return (self._opts['task_python_bin'] or
                self._python_bin())

    def _default_python_bin(self, local=False):
        """The default python command. If local is true, try to use
        sys.executable. Otherwise use 'python2.7' or 'python3' as appropriate.

        This returns a single-item list (because it's a command).
        """
        is_pypy = (python_implementation() == 'PyPy')

        if local and sys.executable:
            return [sys.executable]
        else:
            if PY2:
                return ['pypy'] if is_pypy else ['python2.7']
            else:
                return ['pypy3'] if is_pypy else ['python3']

    ### running MRJob scripts ###

    def _script_args_for_step(self, step_num, mrc, input_manifest=False):
        args = (self._task_python_bin() +
                [self._working_dir_mgr.name('file', self._script_path)] +
                self._args_for_task(step_num, mrc))

        if input_manifest and mrc == 'mapper':
            wrapper = self._manifest_setup_script_path
        elif self._setup_wrapper_script_path:
            wrapper = self._setup_wrapper_script_path
        else:
            return args

        return (self._sh_bin() + [
            self._working_dir_mgr.name('file', wrapper)] + args)

    def _substep_args(self, step_num, mrc):
        step = self._get_step(step_num)

        if step[mrc]['type'] == 'command':
            cmd = step[mrc]['command']

            # never wrap custom hadoop streaming commands in bash
            if isinstance(cmd, string_types):
                return shlex_split(cmd)
            else:
                return cmd

        elif step[mrc]['type'] == 'script':
            script_args = self._script_args_for_step(
                step_num, mrc, input_manifest=step.get('input_manifest'))

            if 'pre_filter' in step[mrc]:
                return self._sh_wrap(
                    '%s | %s' % (step[mrc]['pre_filter'],
                                 cmd_line(script_args)))
            else:
                return script_args
        else:
            raise ValueError("Invalid %s step %d: %r" % (
                mrc, step_num, step[mrc]))

    ### hadoop streaming ###

    def _render_substep(self, step_num, mrc):
        step = self._get_step(step_num)

        if mrc in step:
            # cmd_line() does things that shell is fine with but
            # Hadoop Streaming finds confusing.
            return _hadoop_cmd_line(self._substep_args(step_num, mrc))
        else:
            if mrc == 'mapper':
                return 'cat'
            else:
                return None

    def _hadoop_args_for_step(self, step_num):
        """Build a list of extra arguments to the hadoop binary.

        This handles *cmdenv*, *hadoop_extra_args*, *hadoop_input_format*,
        *hadoop_output_format*, *jobconf*, and *partitioner*.

        This doesn't handle input, output, mappers, reducers, or uploading
        files.
        """
        args = []

        # -libjars, -D
        args.extend(self._hadoop_generic_args_for_step(step_num))

        # hadoop_extra_args (if defined; it's not for sim runners)
        # this has to come after -D because it may include streaming-specific
        # args (see #1332).
        args.extend(self._opts.get('hadoop_extra_args', ()))

        # partitioner
        partitioner = self._partitioner or self._sort_values_partitioner()
        if partitioner:
            args.extend(['-partitioner', partitioner])

        # cmdenv
        for key, value in sorted(self._cmdenv().items()):
            args.append('-cmdenv')
            args.append('%s=%s' % (key, value))

        # hadoop_input_format
        if step_num == 0:
            if self._uses_input_manifest():
                args.extend(['-inputformat', _MANIFEST_INPUT_FORMAT])
            elif self._hadoop_input_format:
                args.extend(['-inputformat', self._hadoop_input_format])

        # hadoop_output_format
        if (step_num == self._num_steps() - 1 and self._hadoop_output_format):
            args.extend(['-outputformat', self._hadoop_output_format])

        return args

    def _hadoop_streaming_jar_args(self, step_num):
        """The arguments that come after ``hadoop jar <streaming jar path>``
        when running a Hadoop streaming job."""
        args = []

        # get command for each part of the job
        mapper, combiner, reducer = (
            self._hadoop_streaming_commands(step_num))

        # set up uploading from HDFS/cloud storage to the working dir
        args.extend(self._upload_args())

        # if no reducer, shut off reducer tasks. This has to come before
        # extra hadoop args, which could contain jar-specific args
        # (e.g. -outputformat). See #1331.
        #
        # might want to just integrate this into _hadoop_args_for_step?
        if not reducer:
            args.extend(['-D', ('%s=0' % translate_jobconf(
                'mapreduce.job.reduces', self.get_hadoop_version()))])

        # Add extra hadoop args first as hadoop args could be a hadoop
        # specific argument which must come before job
        # specific args.
        args.extend(self._hadoop_args_for_step(step_num))

        # set up input
        for input_uri in self._step_input_uris(step_num):
            args.extend(['-input', input_uri])

        # set up output
        args.append('-output')
        args.append(self._step_output_uri(step_num))

        args.append('-mapper')
        args.append(mapper)

        if combiner:
            args.append('-combiner')
            args.append(combiner)

        if reducer:
            args.append('-reducer')
            args.append(reducer)

        return args

    def _hadoop_streaming_commands(self, step_num):
        return (
            self._render_substep(step_num, 'mapper'),
            self._render_substep(step_num, 'combiner'),
            self._render_substep(step_num, 'reducer'),
        )

    def _hadoop_generic_args_for_step(self, step_num):
        """Arguments like -D and -libjars that apply to every Hadoop
        subcommand."""
        args = []

        # libjars (#198)
        libjar_paths = self._libjar_paths()
        if libjar_paths:
            args.extend(['-libjars', ','.join(libjar_paths)])

        # jobconf (-D)
        jobconf = self._jobconf_for_step(step_num)

        for key, value in sorted(jobconf.items()):
            args.extend(['-D', '%s=%s' % (key, value)])

        return args

    def _libjar_paths(self):
        """Paths or URIs of libjars, from Hadoop/Spark's point of view.

        Override this for non-local libjars (e.g. on EMR).
        """
        return self._opts['libjars']

    def _interpolate_jar_step_args(self, args, step_num):
        """Like :py:meth:`_interpolate_step_args` except it
        also replaces `~mrjob.step.GENERIC_ARGS` with
        :py:meth:`_hadoop_generic_args_for_step`. This only
        makes sense for jar steps; Spark should raise an error
        if `~mrjob.step.GENERIC_ARGS` is encountered.
        """
        result = []

        for arg in args:
            if arg == mrjob.step.GENERIC_ARGS:
                result.extend(
                    self._hadoop_generic_args_for_step(step_num))
            else:
                result.append(arg)

        return self._interpolate_step_args(result, step_num)

    ### setup scripts ###

    def _py_files(self):
        """Everything in the *py_files* opt, plus a .zip of the mrjob
        library if needed.
        """
        py_files = list(self._opts['py_files'])

        if self._bootstrap_mrjob():
            py_files.append(self._create_mrjob_zip())

        return py_files

    def _create_setup_wrapper_scripts(self):
        """Create the setup wrapper script, and write it into our local temp
        directory (by default, to a file named setup-wrapper.sh).

        This will set ``self._setup_wrapper_script_path``, and add it to
        ``self._working_dir_mgr``

        This will do nothing if ``self._setup`` is empty or
        this method has already been called.

        If *local* is true, use local line endings (e.g. Windows). Otherwise,
        use UNIX line endings (see #1071).
        """
        if self._has_hadoop_streaming_steps():
            streaming_setup = self._py_files_setup() + self._setup

            if streaming_setup and not self._setup_wrapper_script_path:

                self._setup_wrapper_script_path = self._write_setup_script(
                    streaming_setup, 'setup-wrapper.sh',
                    'streaming setup wrapper script')

            if (self._uses_input_manifest() and not
                    self._manifest_setup_script_path):

                self._manifest_setup_script_path = self._write_setup_script(
                    streaming_setup, 'manifest-setup.sh',
                    'manifest setup wrapper script',
                    manifest=True)

        if (self._has_pyspark_steps() and
                self._spark_executors_have_own_wd() and
                not self._spark_python_wrapper_path):

            pyspark_setup = self._pyspark_setup()
            if pyspark_setup:
                self._spark_python_wrapper_path = self._write_setup_script(
                    pyspark_setup,
                    'python-wrapper.sh', 'Spark Python wrapper script',
                    wrap_python=True)

    def _pyspark_setup(self):
        """Like ``self._setup``, but prepends commands for archive
        emulation if needed."""
        setup = []

        if self._emulate_archives_on_spark():
            for name, path in sorted(
                    self._working_dir_mgr.name_to_path('archive').items()):

                archive_file_name = self._working_dir_mgr.name(
                    'archive_file', path)

                setup.append(_unarchive_cmd(path) % dict(
                    file=pipes.quote(archive_file_name),
                    dir=pipes.quote(name)))

        setup.extend(self._setup)

        return setup

    def _py_files_setup(self):
        """A list of additional setup commands to emulate Spark's
        --py-files option on Hadoop Streaming."""
        result = []

        for py_file in self._py_files():
            path_dict = {'type': 'file', 'name': None, 'path': py_file}
            self._working_dir_mgr.add(**path_dict)
            result.append(['export PYTHONPATH=', path_dict, ':$PYTHONPATH'])

        return result

    def _write_setup_script(self, setup, filename, desc,
                            manifest=False, wrap_python=False):
        """Write a setup script and return its path."""
        contents = self._setup_wrapper_script_content(
            setup, manifest=manifest, wrap_python=wrap_python)

        path = os.path.join(self._get_local_tmp_dir(), filename)
        self._write_script(contents, path, desc)

        self._working_dir_mgr.add('file', path)

        return path

    def _create_mrjob_zip(self):
        """Make a zip of the mrjob library, without .pyc or .pyo files,
        This will also set ``self._mrjob_zip_path`` and return it.

        Typically called from
        :py:meth:`_create_setup_wrapper_scripts`.

        It's safe to call this method multiple times (we'll only create
        the zip file once.)
        """
        if not self._mrjob_zip_path:
            # find mrjob library
            import mrjob

            if not os.path.basename(mrjob.__file__).startswith('__init__.'):
                raise Exception(
                    "Bad path for mrjob library: %s; can't bootstrap mrjob",
                    mrjob.__file__)

            mrjob_dir = os.path.dirname(mrjob.__file__) or '.'

            zip_path = os.path.join(self._get_local_tmp_dir(), 'mrjob.zip')

            def filter_path(path):
                filename = os.path.basename(path)
                return not(filename.lower().endswith('.pyc') or
                           filename.lower().endswith('.pyo') or
                           # filter out emacs backup files
                           filename.endswith('~') or
                           # filter out emacs lock files
                           filename.startswith('.#') or
                           # filter out MacFuse resource forks
                           filename.startswith('._'))

            log.debug('archiving %s -> %s as %s' % (
                mrjob_dir, zip_path, os.path.join('mrjob', '')))
            zip_dir(mrjob_dir, zip_path, filter=filter_path, prefix='mrjob')

            self._mrjob_zip_path = zip_path

        return self._mrjob_zip_path

    def _setup_wrapper_script_content(
            self, setup, manifest=False, wrap_python=False):
        """Return a (Bourne) shell script that runs the setup commands and then
        executes whatever is passed to it (this will be our mapper/reducer),
        as a list of strings (one for each line, including newlines).

        We obtain a file lock so that two copies of the setup commands
        cannot run simultaneously on the same machine (this helps for running
        :command:`make` on a shared source code archive, for example).
        """
        lines = []

        # TODO: this is very similar to _start_of_sh_script() in cloud.py

        if wrap_python:
            # start with shebang
            sh_bin = self._sh_bin()

            if os.path.isabs(sh_bin[0]):
                shebang_bin = sh_bin
            else:
                shebang_bin = ['/usr/bin/env'] + list(sh_bin)

            if len(shebang_bin) > 2:
                # Linux limits shebang to one binary and one arg
                shebang_bin = shebang_bin[:2]
                log.warning('Limiting shebang to two arguments:'
                            '#!%s' % cmd_line(shebang_bin))

            lines.append('#!%s' % cmd_line(shebang_bin))

        # hook for 'set -e', etc.
        pre_commands = self._sh_pre_commands()
        if pre_commands:
            for cmd in pre_commands:
                lines.append(cmd)
            lines.append('')

        if setup:
            lines.extend(self._setup_cmd_content(setup))

        # handle arguments to the script
        if wrap_python:
            # pretend to be python ($@ is arguments to the python binary)
            python_bin = self._task_python_bin()
            lines.append('%s "$@"' % cmd_line(python_bin))
        elif manifest:
            # arguments ($@) are a command
            # eventually runs: "$@" $INPUT_PATH $INPUT_URI
            lines.extend(self._manifest_download_content())
        else:
            # arguments ($@) are a command, just run it
            lines.append('"$@"')

        return lines

    def _setup_cmd_content(self, setup):
        """Write setup script content to obtain a file lock, run setup
        commands in a way that doesn't perturb the script, and then
        release the lock and return to the original working directory."""
        lines = []

        lines.append('# store $PWD')
        lines.append('__mrjob_PWD=$PWD')
        lines.append('')

        lines.append('# obtain exclusive file lock')
        # Basically, we're going to tie file descriptor 9 to our lockfile,
        # use a subprocess to obtain a lock (which we somehow inherit too),
        # and then release the lock by closing the file descriptor.
        # File descriptors 10 and higher are used internally by the shell,
        # so 9 is as out-of-the-way as we can get.
        lines.append('exec 9>/tmp/wrapper.lock.%s' % self._job_key)
        # would use flock(1), but it's not always available
        lines.append("%s -c 'import fcntl; fcntl.flock(9, fcntl.LOCK_EX)'" %
                     cmd_line(self._python_bin()))
        lines.append('')

        lines.append('# setup commands')
        # group setup commands so we can redirect their input/output (see
        # below). Don't use parens; this would invoke a subshell, which would
        # keep us from exporting environment variables to the task.
        lines.append('{')
        for cmd in setup:
            # reconstruct the command line, substituting $__mrjob_PWD/<name>
            # for path dicts
            line = '  '  # indent, since these commands are in a group
            for token in cmd:
                if isinstance(token, dict):
                    # it's a path dictionary
                    line += '$__mrjob_PWD/'
                    line += pipes.quote(self._working_dir_mgr.name(**token))
                else:
                    # it's raw script
                    line += token
            lines.append(line)
        # redirect setup commands' input/output so they don't interfere
        # with the task (see Issue #803).
        lines.append('} 0</dev/null 1>&2')
        lines.append('')

        lines.append('# release exclusive file lock')
        lines.append('exec 9>&-')
        lines.append('')

        lines.append('# run task from the original working directory')
        lines.append('cd $__mrjob_PWD')

        return lines

    def _manifest_download_content(self):
        """write the part of the manifest setup script after setup, that
        downloads the input file, runs the script, and then deletes
        the file."""
        lines = []

        lines.append('{')

        # read URI from stdin
        lines.append('  # read URI of input file from stdin')
        lines.append('  INPUT_URI=$(cut -f 2)')
        lines.append('')

        # pick file extension (e.g. ".warc.gz")
        lines.append('  # pick file extension')
        lines.append("  FILE_EXT=$(basename $INPUT_URI | sed -e 's/^[^.]*//')")
        lines.append('')

        # pick a unique name in the current directory to download the file to
        lines.append('  # pick filename to download to')
        lines.append('  INPUT_PATH=$(mktemp ./input-XXXXXXXXXX$FILE_EXT)')
        lines.append('  rm $INPUT_PATH')
        lines.append('')

        # download the file (using different commands depending on the path)
        lines.append('  # download the input file')
        lines.append('  case $INPUT_URI in')
        download_cmds = (
            list(self._manifest_download_commands()) + [('*', 'cp')])
        for glob, cmd in download_cmds:
            lines.append('    %s)' % glob)
            lines.append('      %s $INPUT_URI $INPUT_PATH' % cmd)
            lines.append('      ;;')
        lines.append('  esac')
        lines.append('')

        # unpack .bz2 and .gz files
        lines.append('  # if input file is compressed, unpack it')
        lines.append('  case $INPUT_PATH in')
        for ext, cmd in self._manifest_uncompress_commands():
            lines.append('    *.%s)' % ext)
            lines.append('      %s $INPUT_PATH' % cmd)
            lines.append("      INPUT_PATH="
                         r"$(echo $INPUT_PATH | sed -e 's/\.%s$//')" % ext)
            lines.append('      ;;')
        lines.append('  esac')
        lines.append('} 1>&2')
        lines.append('')

        # don't exit if script fails
        lines.append('# run our mrjob script')
        lines.append('set +e')
        # pass input path and URI to script
        lines.append('"$@" $INPUT_PATH $INPUT_URI')
        lines.append('')

        # save return code, turn off echo
        lines.append('# if script fails, print input URI before exiting')
        lines.append('{ RETURNCODE=$?; set +x; } 1>&2 2>/dev/null')
        lines.append('')

        lines.append('{')

        # handle errors
        lines.append('  if [ $RETURNCODE -ne 0 ]')
        lines.append('  then')
        lines.append('    echo')
        lines.append('    echo "while reading input from $INPUT_URI"')
        lines.append('  fi')
        lines.append('')

        # clean up input
        lines.append('  rm $INPUT_PATH')
        lines.append('} 1>&2')
        lines.append('')

        # exit with correct status
        lines.append('exit $RETURNCODE')

        return lines

    def _manifest_download_commands(self):
        """Return a list of ``(glob, cmd)``, where *glob*
        matches a path or URI to download, and download command is a command
        to download it (e.g. ```hadoop fs -copyToLocal``), as a
        string.

        Redefine this in your subclass. More specific blobs should come first.
        """
        return []

    def _manifest_uncompress_commands(self):
        """Return a list of ``(ext, cmd)`` where ``ext`` is a file extension
        (e.g. ``gz``) and ``cmd`` is a command to uncompress it (e.g.
        ``gunzip``)."""
        return [
            ('bz2', 'bunzip2'),
            ('gz', 'gunzip'),
        ]

    def _sh_bin(self):
        """The sh binary and any arguments, as a list. Override this
        if, for example, a runner needs different default values
        depending on circumstances (see :py:class:`~mrjob.emr.EMRJobRunner`).
        """
        return self._opts['sh_bin'] or self._default_sh_bin()

    def _default_sh_bin(self):
        """The default sh binary, if :mrjob-opt:`sh_bin` isn't set."""
        return ['/bin/sh', '-ex']

    def _sh_pre_commands(self):
        """A list of lines to put at the very start of any sh script
        (e.g. ``set -e`` when ``sh -e`` wont work, see #1549)
        """
        return []

    def _sh_wrap(self, cmd_str):
        """Helper for _substep_args()

        Wrap command in sh -c '...' to allow for pipes, etc.
        Use *sh_bin* option."""
        # prepend set -e etc.
        cmd_str = '; '.join(self._sh_pre_commands() + [cmd_str])

        return self._sh_bin() + ['-c', cmd_str]

    ### spark ###

    def _args_for_spark_step(self, step_num, last_step_num=None):
        """The actual arguments used to run the spark-submit command.

        This handles both all Spark step types (``spark``, ``spark_jar``,
        and ``spark_script``).

        *last_step_num* is only used by the Spark runner, where multiple
        streaming steps are run in a single Spark job
        """
        return (
            self.get_spark_submit_bin() +
            self._spark_submit_args(step_num) +
            [self._spark_script_path(step_num)] +
            self._spark_script_args(step_num, last_step_num)
        )

    def _run_spark_submit(self, spark_submit_args, env, record_callback):
        """Run the spark submit binary in a subprocess, using a PTY if possible

        :param spark_submit_args: spark-submit binary and arguments, as as list
        :param env: environment variables, as a dict
        :param record_callback: a function that takes a single log4j record
                                as its argument (see
                                :py:func:`~mrjob.logs.log4j\
                                ._parse_hadoop_log4j_records)

        :return: tuple of the subprocess's return code and a
                 step interpretation dictionary
        """
        log.debug('> %s' % cmd_line(spark_submit_args))
        log.debug('  with environment: %r' % sorted(env.items()))

        # these should always be set, but just in case
        returncode = 0
        step_interpretation = {}

        # try to use a PTY if it's available
        try:
            pid, master_fd = pty.fork()
        except (AttributeError, OSError):
            # no PTYs, just use Popen

            # user won't get much feedback for a while, so tell them
            # spark-submit is running
            log.debug('No PTY available, using Popen() to invoke spark-submit')

            step_proc = Popen(
                spark_submit_args, stdout=PIPE, stderr=PIPE, env=env)

            # parse driver output
            step_interpretation = _parse_spark_log(
                step_proc.stderr, record_callback=record_callback)

            # there shouldn't be much output on STDOUT, just echo it
            for record in _parse_hadoop_log4j_records(step_proc.stdout):
                record_callback(record)

            step_proc.stdout.close()
            step_proc.stderr.close()

            returncode = step_proc.wait()
        else:
            # we have PTYs
            if pid == 0:  # we are the child process
                try:
                    os.execvpe(spark_submit_args[0], spark_submit_args, env)
                    # now this process is no longer Python
                except OSError as ex:
                    # use _exit() so we don't do cleanup, etc. that's
                    # the parent process's job
                    os._exit(ex.errno)
                finally:
                    # if we get some other exception, still exit hard
                    os._exit(-1)
            else:
                log.debug('Invoking spark-submit via PTY')

                with os.fdopen(master_fd, 'rb') as master:
                    step_interpretation = (
                        _parse_spark_log(
                            _eio_to_eof(master),
                            record_callback=record_callback))

                    _, returncode = os.waitpid(pid, 0)

        return (returncode, step_interpretation)

    def get_spark_submit_bin(self):
        """Return the location of the ``spark-submit`` binary, searching for it
        if necessary."""
        if not self._spark_submit_bin:
            self._spark_submit_bin = self._find_spark_submit_bin()
        return self._spark_submit_bin

    def _find_spark_submit_bin(self):
        """Attempt to find the spark binary. Returns a list of arguments.
        Defaults to ``['spark-submit']``.

        Re-define this in your subclass if you already know where
        to find spark-submit (e.g. on cloud services).
        """
        for path in unique(self._spark_submit_bin_dirs()):
            log.info('Looking for spark-submit binary in %s...' % (
                path or '$PATH'))

            spark_submit_bin = which('spark-submit', path=path)

            if spark_submit_bin:
                log.info('Found spark-submit binary: %s' % spark_submit_bin)
                return [spark_submit_bin]
        else:
            log.info("Falling back to 'spark-submit'")
            return ['spark-submit']

    def _spark_submit_bin_dirs(self):
        # $SPARK_HOME
        spark_home = os.environ.get('SPARK_HOME')
        if spark_home:
            yield os.path.join(spark_home, 'bin')

        yield None  # use $PATH

        # look for pyspark installation (see #1984)
        if pyspark:
            yield os.path.join(os.path.dirname(pyspark.__file__), 'bin')

        # some other places recommended by install docs (see #1366)
        yield '/usr/lib/spark/bin'
        yield '/usr/local/spark/bin'
        yield '/usr/local/lib/spark/bin'

    def _spark_submit_args(self, step_num):
        """Build a list of extra args to the spark-submit binary for
        the given spark or spark_script step."""
        step = self._get_step(step_num)

        args = []

        # --conf arguments include python bin, cmdenv, jobconf. Make sure
        # that we can always override these manually
        jobconf = {}
        for key, value in self._spark_cmdenv(step_num).items():
            jobconf['spark.executorEnv.%s' % key] = value
            if self._spark_master() == 'yarn':  # YARN only, see #1919
                jobconf['spark.yarn.appMasterEnv.%s' % key] = value

        jobconf.update(self._jobconf_for_step(step_num))

        for key, value in sorted(jobconf.items()):
            args.extend(['--conf', '%s=%s' % (key, value)])

        # add --class (JAR steps)
        if step.get('main_class'):
            args.extend(['--class', step['main_class']])

        # add --jars, if any
        libjar_paths = self._libjar_paths()
        if libjar_paths:
            args.extend(['--jars', ','.join(libjar_paths)])

        # spark-submit treats --master and --deploy-mode as aliases for
        # --conf spark.master=... and --conf spark.deploy-mode=... (see #2032).
        #
        # we never want jobconf to override spark master or deploy mode, so put
        # these switches after --conf

        # add --master
        if self._spark_master():
            args.extend(['--master', self._spark_master()])

        # add --deploy-mode
        if self._spark_deploy_mode():
            args.extend(['--deploy-mode', self._spark_deploy_mode()])

        # --files and --archives
        args.extend(self._spark_upload_args())

        # --py-files (Python only)
        # spark runner can run 'streaming' steps, so just exclude
        # non-Python steps
        if 'jar' not in step['type']:
            py_file_uris = self._py_files()

            if self._upload_mgr:
                # don't assume py_files are in _upload_mgr; for example,
                # spark-submit doesn't need to upload them
                path_to_uri = self._upload_mgr.path_to_uri()
                py_file_uris = [path_to_uri.get(p, p) for p in py_file_uris]

            if py_file_uris:
                args.extend(['--py-files', ','.join(py_file_uris)])

        # spark_args option
        args.extend(self._opts['spark_args'])

        # step spark_args
        if step.get('spark_args'):
            args.extend(step['spark_args'])

        return args

    def _spark_upload_args(self):
        if not self._spark_executors_have_own_wd():
            # don't bother, there's no working dir to upload to
            return []

        return self._upload_args_helper(
            '--files', None,
            '--archives', None,
            always_use_hash=False,
            emulate_archives=self._emulate_archives_on_spark())

    def _spark_script_path(self, step_num):
        """The path of the spark script or JAR, used by
        _args_for_spark_step()."""
        step = self._get_step(step_num)

        if step['type'] == 'spark':
            path = self._script_path
        elif step['type'] == 'spark_jar':
            path = step['jar']
        elif step['type'] == 'spark_script':
            path = step['script']
        else:
            raise TypeError('Bad step type: %r' % step['type'])

        return self._interpolate_spark_script_path(path)

    def _interpolate_spark_script_path(self, path):
        """Redefine this in your subclass if the given path needs to be
        translated to a URI when running spark (e.g. on EMR)."""
        return path

    def _spark_cmdenv(self, step_num):
        """Returns a dictionary mapping environment variable to value,
        including mapping PYSPARK_PYTHON to self._python_bin()
        """
        step = self._get_step(step_num)

        cmdenv = {}

        if self._step_type_uses_pyspark(step['type']):
            driver_python = cmd_line(self._python_bin())

            if self._spark_python_wrapper_path:
                executor_python = './%s' % self._working_dir_mgr.name(
                    'file', self._spark_python_wrapper_path)
            else:
                executor_python = cmd_line(self._task_python_bin())

            if self._spark_deploy_mode() == 'cluster':
                # treat driver like executors (they run in same environment)
                cmdenv['PYSPARK_PYTHON'] = executor_python
            elif driver_python == executor_python:
                # no difference, just set $PYSPARK_PYTHON
                cmdenv['PYSPARK_PYTHON'] = driver_python
            else:
                # set different pythons for driver and executor
                cmdenv['PYSPARK_PYTHON'] = executor_python
                cmdenv['PYSPARK_DRIVER_PYTHON'] = driver_python

        cmdenv.update(self._cmdenv())
        return cmdenv


# these don't need to be methods

def _hadoop_cmd_line(args):
    """Escape args of a command line in a way that Hadoop can process
    them."""
    return ' '.join(_hadoop_escape_arg(arg) for arg in args)


def _hadoop_escape_arg(arg):
    """Escape a single command argument in a way that Hadoop can process it."""
    if _HADOOP_SAFE_ARG_RE.match(arg):
        return arg
    else:
        return "'%s'" % arg.replace("'", r"'\''")
