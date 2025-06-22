# Copyright 2019 Yelp and Google, Inc.
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
"""A runner that can run jobs on Spark, with or without Hadoop."""
import json
import logging
import os.path
import posixpath
import re
from copy import deepcopy
from subprocess import CalledProcessError
from tempfile import gettempdir

from mrjob.bin import MRJobBinRunner
from mrjob.cloud import _DEFAULT_CLOUD_PART_SIZE_MB
from mrjob.conf import combine_dicts
from mrjob.compat import jobconf_from_dict
from mrjob.dataproc import _DEFAULT_CLOUD_TMP_DIR_OBJECT_TTL_DAYS
from mrjob.fs.composite import CompositeFilesystem
from mrjob.fs.gcs import GCSFilesystem
from mrjob.fs.gcs import google as google_libs_installed
from mrjob.fs.gcs import _is_permanent_google_error
from mrjob.fs.hadoop import HadoopFilesystem
from mrjob.fs.local import LocalFilesystem
from mrjob.fs.s3 import S3Filesystem
from mrjob.fs.s3 import boto3 as boto3_installed
from mrjob.fs.s3 import _is_permanent_boto3_error
from mrjob.hadoop import fully_qualify_hdfs_path
from mrjob.logs.counters import _format_counters
from mrjob.logs.errors import _log_probable_cause_of_failure
from mrjob.logs.errors import _pick_error
from mrjob.logs.step import _log_log4j_record
from mrjob.parse import is_uri
from mrjob.parse import to_uri
from mrjob.py2 import to_unicode
from mrjob.setup import UploadDirManager
from mrjob.step import StepFailedException
from mrjob.util import cmd_line
from mrjob.util import _create_zip_file

log = logging.getLogger(__name__)

_CLOSE_BRACE_AFTER_CLOSE_BRACE_RE = re.compile(r'(?<=\})\}')


class SparkMRJobRunner(MRJobBinRunner):
    """Runs a :py:class:`~mrjob.job.MRJob` on your Spark cluster (with or
    without Hadoop). Invoked when you run your job with ``-r spark``.

    See :ref:`running-on-your-spark-cluster` for more information.

    The Spark runner can also run "classic" MRJobs directly on Spark, without
    using Hadoop streaming. See :ref:`classic-mrjobs-on-spark`.

    .. versionadded:: 0.6.8
    """
    alias = 'spark'

    # other than ``spark_*``, these options are only used for filesystems
    #
    # max_output_files doesn't appear here because it can only be read from
    # the command line, not mrjob.conf (see #2040)
    OPT_NAMES = MRJobBinRunner.OPT_NAMES | {
        'aws_access_key_id',
        'aws_secret_access_key',
        'aws_session_token',
        'cloud_fs_sync_secs',
        'cloud_part_size_mb',
        'emulate_map_input_file',
        'gcs_region',  # used when creating buckets on GCS
        'hadoop_bin',
        'project_id',  # used by GCS filesystem
        's3_endpoint',
        's3_region',  # used when creating buckets on S3
        'skip_internal_protocol',
        'spark_deploy_mode',
        'spark_master',
        'spark_tmp_dir',  # where to put temp files in Spark
    }

    # everything except Hadoop JARs
    # streaming jobs will be run using mrjob/spark/harness.py (see #1972)
    _STEP_TYPES = {
        'spark', 'spark_jar', 'spark_script', 'streaming',
    }

    def __init__(self, max_output_files=None, mrjob_cls=None, **kwargs):
        """Create a Spark runner.

        :param max_output_files: limit on number of output files when
                                 running streaming jobs. Can only be
                                 set on command line (not config file)
        :param mrjob_cls: class of the job you want to run. Used for
                          running streaming steps in Spark
        """
        # need to set this before checking steps in superclass __init__()
        self._mrjob_cls = mrjob_cls

        super(SparkMRJobRunner, self).__init__(**kwargs)

        self._max_output_files = max_output_files

        if self._opts['spark_tmp_dir']:
            self._check_spark_tmp_dir_opt()

        self._spark_tmp_dir = self._pick_spark_tmp_dir()

        # where local files are uploaded into Spark
        if is_uri(self._spark_tmp_dir):
            spark_files_dir = posixpath.join(self._spark_tmp_dir, 'files', '')
            self._upload_mgr = UploadDirManager(spark_files_dir)

        # where to put job output (if not set explicitly)
        if not self._output_dir:
            self._output_dir = self.fs.join(self._spark_tmp_dir, 'output')

        # keep track of where the spark-submit binary is
        self._spark_submit_bin = self._opts['spark_submit_bin']

        # where to store a .zip file containing the MRJob, with a unique
        # module name
        self._job_script_zip_path = None

        # counters, one per job step. (Counters will be {} for non-streaming
        # steps because Spark doesn't have counters).
        self._counters = []

        # TODO: we may eventually want log interpretation, but it shouldn't
        # include counters, as they are not found in logs.

    def _check_spark_tmp_dir_opt(self):
        # warn if spark_tmp_dir isn't actually visible to Spark executors
        # (see #2062)
        tmp_dir_is_local = to_uri(
            self._opts['spark_tmp_dir']).startswith('file://')
        spark_master_is_local = self._spark_master().startswith('local')

        if tmp_dir_is_local != spark_master_is_local:
            log.warning(
                'Warning: executors on Spark master %s may not be able to'
                ' access spark_tmp_dir %s' %
                (self._spark_master(), self._opts['spark_tmp_dir']))

    def _check_step(self, step, step_num):
        """Don't try to run steps that include commands or use manifests."""
        super(SparkMRJobRunner, self)._check_step(step, step_num)

        if step.get('input_manifest'):
            raise NotImplementedError(
                'spark runner does not support input manifests')

        # we don't currently support commands, but we *could* (see #1956).
        if step['type'] == 'streaming':
            if not self._mrjob_cls:
                raise ValueError(
                    'You must set mrjob_cls to run streaming steps')

            for mrc in ('mapper', 'combiner', 'reducer'):
                if step.get(mrc):
                    if 'command' in step[mrc] or 'pre_filter' in step[mrc]:
                        raise NotImplementedError(
                            "step %d's %s runs a command, but spark"
                            " runner does not support commands" % (
                                step_num, mrc))

    @classmethod
    def _default_opts(cls):
        return combine_dicts(
            super(SparkMRJobRunner, cls)._default_opts(),
            dict(
                cloud_part_size_mb=_DEFAULT_CLOUD_PART_SIZE_MB,
            ),
        )

    def _run(self):
        self.get_spark_submit_bin()  # find spark-submit up front
        self._create_setup_wrapper_scripts()
        self._upload_local_files()
        self._run_steps_on_spark()

    def _pick_spark_tmp_dir(self):
        if self._opts['spark_tmp_dir']:
            return self.fs.join(self._opts['spark_tmp_dir'], self._job_key)
        else:
            master = self._spark_master() or 'local'
            if master.startswith('local'):  # including local-cluster
                # need a local temp dir
                # add "-spark" so we don't collide with default local temp dir
                return os.path.join(
                    gettempdir(), self._job_key + '-spark')
            else:
                # use HDFS (same default as HadoopJobRunner)
                return posixpath.join(
                    fully_qualify_hdfs_path('tmp/mrjob'), self._job_key)

    def _default_step_output_dir(self):
        return self.fs.join(self._spark_tmp_dir, 'step-output')

    def _counter_output_dir(self, step_num):
        return self.fs.join(
            self._spark_tmp_dir, 'counter-output-step-%d' % step_num)

    def counters(self):
        return deepcopy(self._counters)

    @property
    def fs(self):
        # Spark supports basically every filesystem there is

        if not self._fs:
            self._fs = CompositeFilesystem()

            if boto3_installed:
                self._fs.add_fs('s3', S3Filesystem(
                    aws_access_key_id=self._opts['aws_access_key_id'],
                    aws_secret_access_key=self._opts['aws_secret_access_key'],
                    aws_session_token=self._opts['aws_session_token'],
                    s3_endpoint=self._opts['s3_endpoint'],
                    s3_region=self._opts['s3_region'],
                ), disable_if=_is_permanent_boto3_error)

            if google_libs_installed:
                self._fs.add_fs('gcs', GCSFilesystem(
                    project_id=self._opts['project_id'],
                    location=self._opts['gcs_region'],
                    object_ttl_days=_DEFAULT_CLOUD_TMP_DIR_OBJECT_TTL_DAYS,
                ), disable_if=_is_permanent_google_error)

            # Hadoop FS is responsible for all URIs that fall through to it
            self._fs.add_fs('hadoop', HadoopFilesystem(
                self._opts['hadoop_bin']))

            self._fs.add_fs('local', LocalFilesystem())

        return self._fs

    # making mr_job_script visible in Spark

    def _job_script_module_name(self):
        """A unique module name to use with the MRJob script."""
        return re.sub(r'[^\w\d]', '_', self._job_key)

    def _create_job_script_zip(self):
        if not self._job_script_zip_path:
            zip_path = os.path.join(self._get_local_tmp_dir(), 'script.zip')
            name_in_zip = self._job_script_module_name() + '.py'

            log.debug('archiving %s -> %s as %s' % (
                self._script_path, zip_path, name_in_zip))
            with _create_zip_file(zip_path) as zip_file:
                zip_file.write(self._script_path, arcname=name_in_zip)

            self._job_script_zip_path = zip_path

        return self._job_script_zip_path

    def _py_files(self):
        """Patch in :py:attr:`_job_script_zip_path`, if running streaming
        steps."""
        py_files = super(SparkMRJobRunner, self)._py_files()

        if self._has_streaming_steps():
            py_files.append(self._create_job_script_zip())

        return py_files

    # running the job

    def _run_steps_on_spark(self):
        steps = self._get_steps()

        for group in self._group_steps(steps):
            step_num = group['step_num']
            last_step_num = step_num + len(group['steps']) - 1

            # the Spark harness can run several streaming steps in one job
            if step_num == last_step_num:
                step_desc = 'step %d' % (step_num + 1)
            else:
                step_desc = 'steps %d-%d' % (step_num + 1, last_step_num + 1)

            log.info('Running %s of %d' % (step_desc, len(steps)))

            self._run_step_on_spark(group['steps'][0], step_num, last_step_num)

    def _group_steps(self, steps):
        """Group streaming steps together."""
        # a list of dicts with:
        #
        # type -- shared type of steps
        # steps -- list of steps in group
        # step_num -- (0-indexed) number of first step
        groups = []

        for step_num, step in enumerate(steps):
            # should we add *step* to existing group of streaming steps?
            if (step['type'] == 'streaming' and groups and
                    groups[-1]['type'] == 'streaming' and
                    step.get('jobconf') ==
                    groups[-1]['steps'][0].get('jobconf')):
                groups[-1]['steps'].append(step)
            else:
                # start a new step group
                groups.append(dict(
                    type=step['type'],
                    steps=[step],
                    step_num=step_num))

        return groups

    def _run_step_on_spark(self, step, step_num, last_step_num=None):
        spark_submit_args = self._args_for_spark_step(step_num, last_step_num)

        env = dict(os.environ)
        env.update(self._spark_cmdenv(step_num))

        returncode, step_interpretation = self._run_spark_submit(
            spark_submit_args, env, record_callback=_log_log4j_record)

        counters = None
        if step['type'] == 'streaming':
            counter_file = self.fs.join(
                self._counter_output_dir(step_num), 'part-*')
            counter_json = b''.join(self.fs.cat(counter_file))
            if counter_json.strip():
                # json.loads() on Python 3.4/3.5 can't take bytes
                counters = json.loads(to_unicode(counter_json))

        if isinstance(counters, list):
            self._counters.extend(counters)

            # desc_num is 1-indexed user-readable step num
            for desc_num, counter_dict in enumerate(
                    counters, start=(step_num + 1)):
                if counter_dict:
                    log.info(_format_counters(
                        counter_dict,
                        desc=('Counters for step %d' % desc_num)))

        # for non-streaming steps, there are no counters.
        # pad self._counters to match number of steps
        while len(self._counters) < (last_step_num or step_num) + 1:
            self._counters.append({})

        if returncode:
            error = _pick_error(dict(step=step_interpretation))
            if error:
                _log_probable_cause_of_failure(log, error)

            reason = str(CalledProcessError(returncode, spark_submit_args))
            raise StepFailedException(
                reason=reason, step_num=step_num, last_step_num=last_step_num,
                num_steps=self._num_steps())

    def _spark_script_path(self, step_num):
        """For streaming steps, return the path of the harness script
        (and handle other spark step types the usual way)."""
        step = self._get_step(step_num)

        if step['type'] == 'streaming':
            return self._spark_harness_path()
        else:
            return super(SparkMRJobRunner, self)._spark_script_path(step_num)

    def _spark_script_args(self, step_num, last_step_num=None):
        """Generate spark harness args for streaming steps (and handle
        other spark step types the usual way).
        """
        if last_step_num is None:
            last_step_num = step_num

        steps = self._get_steps()[step_num:last_step_num + 1]

        if steps[0]['type'] != 'streaming':
            return super(SparkMRJobRunner, self)._spark_script_args(
                step_num, last_step_num)

        args = []

        # class name
        args.append('%s.%s' % (self._job_script_module_name(),
                               self._mrjob_cls.__name__))

        # INPUT
        args.append(
            ','.join(self._step_input_uris(step_num)))

        # OUTPUT
        # note that we use the output dir for the *last* step
        args.append(
            self._step_output_uri(last_step_num))

        # --hadoop-input-format
        if self._hadoop_input_format:
            args.extend(['--hadoop-input-format', self._hadoop_input_format])
        else:
            # you can't pass --hadoop-input-format '' to EMR's script runner,
            # so pass something that doesn't use an empty string (see #2055)
            args.append('--no-hadoop-input-format')

        # --hadoop-output-format
        if self._hadoop_output_format:
            args.extend(['--hadoop-output-format', self._hadoop_output_format])
        else:
            # alternative to --hadoop-output-format '' (see #2055)
            args.append('--no-hadoop-output-format')

        # --sort-values
        if self._sort_values:
            args.append('--sort-values')
        else:
            args.append('--no-sort-values')

        # --steps-desc
        args.extend(['--steps-desc',
                     _emr_proof_steps_desc(json.dumps(steps))])

        # --counter-output-dir, to simulate counters
        args.extend(['--counter-output-dir',
                     self._counter_output_dir(step_num)])

        # --first-step-num, --last-step-num (step range)
        args.extend(['--first-step-num', str(step_num),
                     '--last-step-num', str(last_step_num)])

        # --job-args (passthrough args)

        # if on local[*] master, keep file upload args as-is (see #2031)
        job_args = self._mr_job_extra_args(
            local=not self._spark_executors_have_own_wd())

        if job_args:
            args.extend(['--job-args', cmd_line(job_args)])

        # --compression-codec
        jobconf = self._jobconf_for_step(step_num)

        compress_conf = jobconf_from_dict(
            jobconf, 'mapreduce.output.fileoutputformat.compress')
        codec_conf = jobconf_from_dict(
            jobconf, 'mapreduce.output.fileoutputformat.compress.codec')

        if compress_conf and compress_conf != 'false' and codec_conf:
            args.extend(['--compression-codec', codec_conf])

        # --num-reducers
        num_reducers = jobconf_from_dict(jobconf, 'mapreduce.job.reduces')
        if num_reducers and int(num_reducers) > 0:
            args.extend(['--num-reducers', str(num_reducers)])

        # --max-output-files
        if self._max_output_files:
            args.extend(['--max-output-files',
                         str(self._max_output_files)])

        # --emulate-map-input-file
        if self._opts['emulate_map_input_file']:
            args.append('--emulate-map-input-file')

        # --skip_internal-protocol
        if self._opts['skip_internal_protocol']:
            args.append('--skip-internal-protocol')

        return args

    def _spark_harness_path(self):
        """Where to find the Spark harness."""
        # harness requires pyspark, which may be in spark-submit's PYTHONPATH
        # but not ours. So don't import the harness unless we need it.
        # (See #2091)
        import mrjob.spark.harness
        path = mrjob.spark.harness.__file__
        if path.endswith('.pyc'):
            path = path[:-1]
        return path

    # "streaming" steps run on Spark too

    def _has_spark_steps(self):
        """Treat streaming steps as Spark steps."""
        return (super(SparkMRJobRunner, self)._has_spark_steps() or
                self._has_streaming_steps())

    def _has_hadoop_streaming_steps(self):
        # the Spark runner doesn't run "streaming" steps on Hadoop
        return False

    def _has_streaming_steps(self):
        """Are any of our steps "streaming" steps that would normally run
        on Hadoop Streaming?"""
        return any(step['type'] == 'streaming' for step in self._get_steps())

    def _step_type_uses_pyspark(self, step_type):
        """Treat streaming steps as Spark steps that use Python."""
        return (
            super(SparkMRJobRunner, self)._step_type_uses_pyspark(step_type) or
            step_type == 'streaming')

    def _step_type_uses_spark(self, step_type):
        """Treat streaming steps as Spark steps that use Python."""
        return (
            super(SparkMRJobRunner, self)._step_type_uses_spark(step_type) or
            step_type == 'streaming')


def _emr_proof_steps_desc(steps_desc):
    # EMR's command-runner.jar does some very strange things to
    # arguments, including deleting empty args and deleting
    # '}}' from arguments. See #2070
    return _CLOSE_BRACE_AFTER_CLOSE_BRACE_RE.sub(' }', steps_desc)
