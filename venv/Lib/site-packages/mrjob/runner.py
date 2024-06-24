# -*- coding: utf-8 -*-
# Copyright 2009-2017 Yelp and Contributors
# Copyright 2018 Yelp and Google, Inc.
# Copyright 2019 Yelp
# Copyright 2020 Affirm, Inc.
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
"""Base class for all runners."""
import copy
import datetime
import getpass
import logging
import os
import os.path
import posixpath
import pprint
import re
import sys
import tarfile
import tempfile
from shutil import rmtree

from mrjob.compat import translate_jobconf
from mrjob.compat import translate_jobconf_dict
from mrjob.compat import translate_jobconf_for_all_versions
from mrjob.conf import ClearedValue
from mrjob.conf import combine_jobconfs
from mrjob.conf import combine_opts
from mrjob.conf import load_opts_from_mrjob_confs
from mrjob.fs.composite import CompositeFilesystem
from mrjob.fs.local import LocalFilesystem
from mrjob.options import _combiners
from mrjob.options import _deprecated_aliases
from mrjob.options import CLEANUP_CHOICES
from mrjob.parse import is_uri
from mrjob.parse import to_uri
from mrjob.py2 import PY2
from mrjob.py2 import string_types
from mrjob.setup import WorkingDirManager
from mrjob.setup import name_uniquely
from mrjob.setup import parse_legacy_hash_path
from mrjob.step import INPUT
from mrjob.step import OUTPUT
from mrjob.step import _is_spark_step_type
from mrjob.step import _is_pyspark_step_type


log = logging.getLogger(__name__)

# use to detect globs and break into the part before and after the glob
GLOB_RE = re.compile(r'^(.*?)([\[\*\?].*)$')

# buffer for piping files into sort on Windows
_BUFFER_SIZE = 4096

# jobconf options for implementing SORT_VALUES
_SORT_VALUES_JOBCONF = {
    'mapreduce.partition.keypartitioner.options': '-k1,1',
    'stream.num.map.output.key.fields': 2
}

# partitioner for sort_values
_SORT_VALUES_PARTITIONER = \
    'org.apache.hadoop.mapred.lib.KeyFieldBasedPartitioner'


class MRJobRunner(object):
    """Abstract base class for all runners"""

    # this class handles the basic runner framework, options and config files,
    # arguments to mrjobs, and setting up job working dirs and environments.
    # this will put files from setup scripts, py_files, and bootstrap_mrjob
    # into the job's working dir, but won't actually run/import them
    #
    # command lines to run substeps (including Spark) are handled by
    # mrjob.bin.MRJobBinRunner

    #: alias for this runner, used on the command line with ``-r``
    alias = None

    # libjars is only here because the job can set it; might want to
    # handle this with a warning from the launcher instead
    OPT_NAMES = {
        'bootstrap_mrjob',
        'check_input_paths',
        'cleanup',
        'cleanup_on_failure',
        'cmdenv',
        'jobconf',
        'label',
        'libjars',
        'local_tmp_dir',
        # no max_output_files because it doesn't go in self._opts
        'owner',
        'py_files',
        'read_logs',
        'setup',
        'upload_archives',
        'upload_dirs',
        'upload_files'
    }

    # re-define this as a set of step types supported by your runner
    _STEP_TYPES = None

    ### methods to call from your batch script ###

    def __init__(self, mr_job_script=None, conf_paths=None,
                 extra_args=None,
                 hadoop_input_format=None, hadoop_output_format=None,
                 input_paths=None, output_dir=None, partitioner=None,
                 sort_values=None, stdin=None, steps=None,
                 step_output_dir=None,
                 **opts):
        """All runners take the following keyword arguments:

        :type mr_job_script: str
        :param mr_job_script: the path of the ``.py`` file containing the
                              :py:class:`~mrjob.job.MRJob`. If this is None,
                              you won't actually be able to :py:meth:`run` the
                              job, but other utilities (e.g. :py:meth:`ls`)
                              will work.
        :type conf_paths: None or list
        :param conf_paths: List of config files to combine and use, or None to
                           search for mrjob.conf in the default locations.
        :type extra_args: list of str
        :param extra_args: a list of extra cmd-line arguments to pass to the
                           mr_job script. This is a hook to allow jobs to take
                           additional arguments.
        :type hadoop_input_format: str
        :param hadoop_input_format: name of an optional Hadoop ``InputFormat``
                                    class. Passed to Hadoop along with your
                                    first step with the ``-inputformat``
                                    option. Note that if you write your own
                                    class, you'll need to include it in your
                                    own custom streaming jar (see
                                    :mrjob-opt:`hadoop_streaming_jar`).
        :type hadoop_output_format: str
        :param hadoop_output_format: name of an optional Hadoop
                                     ``OutputFormat`` class. Passed to Hadoop
                                     along with your first step with the
                                     ``-outputformat`` option. Note that if you
                                     write your own class, you'll need to
                                     include it in your own custom streaming
                                     jar (see
                                     :mrjob-opt:`hadoop_streaming_jar`).
        :type input_paths: list of str
        :param input_paths: Input files for your job. Supports globs and
                            recursively walks directories (e.g.
                            ``['data/common/', 'data/training/*.gz']``). If
                            this is left blank, we'll read from stdin
        :type output_dir: str
        :param output_dir: An empty/non-existent directory where Hadoop
                           should put the final output from the job.
                           If you don't specify an output directory, we'll
                           output into a subdirectory of this job's temporary
                           directory. You can control this from the command
                           line with ``--output-dir``. This option cannot be
                           set from configuration files. If used with the
                           hadoop runner, this path does not need to be fully
                           qualified with ``hdfs://`` URIs because it's
                           understood that it has to be on HDFS.
        :type partitioner: str
        :param partitioner: Optional name of a Hadoop partitioner class, e.g.
                            ``'org.apache.hadoop.mapred.lib.HashPartitioner'``.
                            Hadoop streaming will use this to determine how
                            mapper output should be sorted and distributed
                            to reducers.
        :type sort_values: bool
        :param sort_values: if true, set partitioners and jobconf variables
                            so that reducers to receive the values
                            associated with any key in sorted order (sorted by
                            their *encoded* value). Also known as secondary
                            sort.
        :param stdin: an iterable (can be a ``BytesIO`` or even a list) to use
                      as stdin. This is a hook for testing; if you set
                      ``stdin`` via :py:meth:`~mrjob.job.MRJob.sandbox`, it'll
                      get passed through to the runner. If for some reason
                      your lines are missing newlines, we'll add them;
                      this makes it easier to write automated tests.
        :param steps: a list of descriptions of steps to run (see :doc:`step`
                      for description formats)
        :type step_output_dir: str
        :param step_output_dir: An empty/non-existent directory where Hadoop
                                should put output from all steps other than
                                the last one (this only matters for multi-step
                                jobs). Currently ignored by local runners.
        """
        self._ran_job = False

        # opts are made from:
        #
        # empty defaults (everything set to None)
        # runner-specific defaults
        # opts from config file(s)
        # opts from command line
        self._opts = self._combine_confs(
            [(None, {key: None for key in self.OPT_NAMES})] +
            [(None, self._default_opts())] +
            load_opts_from_mrjob_confs(self.alias, conf_paths) +
            [('the command line', opts)]
        )

        log.debug('Active configuration:')
        log.debug(pprint.pformat({
            opt_key: self._obfuscate_opt(opt_key, opt_value)
            for opt_key, opt_value in self._opts.items()
        }))

        self._fs = None

        # a local tmp directory that will be cleaned up when we're done
        # access/make this using self._get_local_tmp_dir()
        self._local_tmp_dir = None

        if self._emulate_archives_on_spark():
            # keep Spark from auto-uncompressing tarballs
            archive_file_suffix = '.file'
        else:
            # otherwise, leave as-is, so that --archive will
            # work properly
            archive_file_suffix = ''

        self._working_dir_mgr = WorkingDirManager(
            archive_file_suffix=archive_file_suffix)

        # mapping from dir to path for corresponding archive. we pick
        # paths during init(), but don't actually create the archives
        # until self._create_dir_archives() is called
        self._dir_to_archive_path = {}
        # dir archive names (the filename minus ".tar.gz") already taken
        self._dir_archive_names_taken = set()
        # set of dir_archives that have actually been created
        self._dir_archives_created = set()

        # set this to an :py:class:`~mrjob.setup.UploadDirManager` in
        # runners that upload files to HDFS, S3, etc.
        #
        # this manager should not handle files belonging to
        # self._working_dir_mgr,
        # which, if they are uploaded, will go into self._wd_upload_dir()
        self._upload_mgr = None

        self._script_path = mr_job_script
        if self._script_path:
            self._working_dir_mgr.add('file', self._script_path)

        # give this job a unique name
        self._job_key = self._make_unique_job_key()

        # extra args to our job
        self._extra_args = list(extra_args) if extra_args else []
        for extra_arg in self._extra_args:
            if isinstance(extra_arg, dict):
                if extra_arg.get('type') != 'file':
                    raise NotImplementedError
                self._working_dir_mgr.add(**extra_arg)

        # set up uploading
        for hash_path in self._opts['upload_files']:
            uf = parse_legacy_hash_path('file', hash_path,
                                        must_name='upload_files')
            self._working_dir_mgr.add(**uf)

        for hash_path in self._opts['upload_archives']:
            ua = parse_legacy_hash_path('archive', hash_path,
                                        must_name='upload_archives')
            self._working_dir_mgr.add(**ua)

        for hash_path in self._opts['upload_dirs']:
            # pick name based on directory path
            ud = parse_legacy_hash_path('dir', hash_path,
                                        must_name='upload_archives')
            # but feed working_dir_mgr the archive's path
            archive_path = self._dir_archive_path(ud['path'])
            self._working_dir_mgr.add(
                'archive', archive_path, name=ud['name'])

        # Where to read input from (log files, etc.)
        self._input_paths = input_paths or ['-']  # by default read from stdin
        if PY2:
            self._stdin = stdin or sys.stdin
        else:
            self._stdin = stdin or sys.stdin.buffer
        self._stdin_path = None  # temp file containing dump from stdin

        # where to keep the input manifest
        self._input_manifest_path = None

        # store output_dir
        self._output_dir = output_dir

        # store partitioner
        self._partitioner = partitioner

        # store sort_values
        self._sort_values = sort_values

        # store step_output_dir
        self._step_output_dir = step_output_dir

        # store hadoop input and output formats
        self._hadoop_input_format = hadoop_input_format
        self._hadoop_output_format = hadoop_output_format

        # check and store *steps*
        self._steps = []
        if steps:
            self._check_steps(steps)
            self._steps = copy.deepcopy(steps)

        # this variable marks whether a cleanup has happened and this runner's
        # output stream is no longer available.
        self._closed = False

    ### Options ####

    @classmethod
    def _default_opts(cls):
        try:
            owner = getpass.getuser()
        except:
            owner = None

        return dict(
            check_input_paths=True,
            cleanup=['ALL'],
            cleanup_on_failure=['NONE'],
            owner=owner,
        )

    def _combine_confs(self, source_and_opt_list):
        """Combine several opt dictionaries into one.

        *source_and_opt_list* is a list of tuples of *source*,
        *opts* where *opts* is a dictionary and *source* is either
        None or a description of where the opts came from (usually a path).

        Only override this if you need truly fine-grained control,
        including knowledge of the options' source.
        """
        opt_list = [
            self._fix_opts(opts, source)
            for source, opts in source_and_opt_list
        ]

        return self._combine_opts(opt_list)

    def _combine_opts(self, opt_list):
        """Combine several opt dictionaries into one. *opt_list*
        is a list of dictionaries containing validated options

        Override this if you need to base options off the values of
        other options, but don't need to issue warnings etc.
        about the options' source.
        """
        return combine_opts(self._opt_combiners(), *opt_list)

    def _opt_combiners(self):
        """A dictionary mapping opt name to combiner funciton. This
        won't necessarily include every opt name (we default to
        :py:func:`~mrjob.conf.combine_value`).
        """
        return _combiners(self.OPT_NAMES)

    def _fix_opts(self, opts, source=None):
        """Take an options dictionary, and either return a sanitized
        version of it, or raise an exception.

        *source* is either a string describing where the opts came from
        or None.

        This ensures that opt dictionaries are really dictionaries
        and handles deprecated options.
        """
        if source is None:
            source = 'defaults'  # defaults shouldn't trigger warnings

        if not isinstance(opts, dict):
            raise TypeError(
                'options for %s (from %s) must be a dict' %
                (self.alias, source))

        deprecated_aliases = _deprecated_aliases(self.OPT_NAMES)

        results = {}

        for k, v in sorted(opts.items()):
            # rewrite deprecated aliases
            if k in deprecated_aliases:
                if v is None:  # don't care
                    continue

                aliased_opt = deprecated_aliases

                log.warning('Deprecated option %s (from %s) has been renamed'
                            ' to %s and will be removed in v0.7.0' % (
                                k, source, aliased_opt))

                if opts.get(aliased_opt) is not None:
                    return  # don't overwrite non-aliased opt

                k = aliased_opt

            if k in self.OPT_NAMES:
                if v is None:
                    fixed_v = None
                elif isinstance(v, ClearedValue):
                    # _fix_opt() doesn't need to know about !clear (see #2102)
                    fixed_v = ClearedValue(self._fix_opt(k, v.value, source))
                else:
                    fixed_v = self._fix_opt(k, v, source)

                results[k] = fixed_v
            elif v:
                log.warning('Unexpected option %s (from %s)' % (k, source))

        return results

    def _fix_opt(self, opt_key, opt_value, source):
        """Fix a single option, returning its correct value or raising
        an exception. This is not called for options that are ``None``.

        This currently handles cleanup opts.

        Override this if you require additional opt validation or cleanup.
        """
        if opt_key in ('cleanup', 'cleanup_on_failure'):
            return self._fix_cleanup_opt(opt_key, opt_value, source)
        else:
            return opt_value

    def _fix_cleanup_opt(self, opt_key, opt_value, source):
        """Fix a cleanup option, or raise ValueError."""
        if isinstance(opt_value, string_types):
            opt_value = [opt_value]

        if 'NONE' in opt_value and len(set(opt_value)) > 1:
            raise ValueError(
                'Cannot clean up both nothing and something!'
                ' (%s option from %s)' % (opt_key, source))

        for cleanup_type in opt_value:
            if cleanup_type not in CLEANUP_CHOICES:
                raise ValueError(
                    '%s must be one of %s, not %s (from %s)' % (
                        opt_key, ', '.join(CLEANUP_CHOICES), opt_value,
                        source))

        return opt_value

    def _obfuscate_opt(self, opt_key, opt_value):
        """Return value of opt to show in debug printout. Used to obfuscate
        credentials, etc."""
        return opt_value

    ### Filesystem object ###

    @property
    def fs(self):
        """:py:class:`~mrjob.fs.base.Filesystem` object for the local
        filesystem.
        """
        if self._fs is None:
            # wrap LocalFilesystem in LocalFilesystem to get IOError
            # on URIs (see #1185)
            self._fs = CompositeFilesystem()
            self._fs.add_fs('local', LocalFilesystem())
        return self._fs

    ### Running the job and parsing output ###

    def run(self):
        """Run the job, and block until it finishes.

        Raise :py:class:`~mrjob.step.StepFailedException` if there
        are any problems (except on
        :py:class:`~mrjob.inline.InlineMRJobRunner`, where we raise the
        actual exception that caused the step to fail).
        """
        if self._ran_job:
            raise ValueError('Job already ran!')

        if self._num_steps() == 0:
            raise ValueError('Job has no steps!')

        self._create_dir_archives()
        # TODO: no point in checking input paths if we're going to
        # make a manifest out of them
        self._check_input_paths()
        self._add_input_files_for_upload()
        self._create_input_manifest_if_needed()
        self._run()
        self._ran_job = True

        last_step = self._get_steps()[-1]

        # only print this message if the last step uses our output dir
        if 'args' not in last_step or OUTPUT in last_step['args']:
            log.info('job output is in %s' % self._output_dir)

    def cat_output(self):
        """Stream the job's output, as a stream of ``bytes``. If there are
        multiple output files, there will be an empty bytestring
        (``b''``) between them.

        Like Hadoop input formats, we ignore files and subdirectories whose
        names start with ``"_"`` or ``"."`` (e.g. ``_SUCCESS``, ``_logs/``,
        ``.part-00000.crc``.

        .. versionchanged:: 0.6.8

           Ignore file/dirnames starting with ``"."`` as well as ``"_"``.
        """
        output_dir = self.get_output_dir()
        if output_dir is None:
            raise ValueError('Run the job before streaming output')

        if self._closed is True:
            log.warning(
                'WARNING! Trying to stream output from a closed runner, output'
                ' will probably be empty.')

        log.info('Streaming final output from %s...' % output_dir)

        def split_path(path):
            while True:
                base, name = os.path.split(path)

                # no more elements
                if not name:
                    break

                yield name

                path = base

        def ls_output():
            for filename in self.fs.ls(output_dir):
                subpath = filename[len(output_dir):]
                # Hadoop ignores files and dirs inside the output dir
                # whose names start with '_' or '.'. See #1337.
                if not (any(name[0] in '_.'
                            for name in split_path(subpath))):
                    yield filename

        for i, filename in enumerate(ls_output()):
            if i > 0:
                yield b''  # EOF of previous file

            for chunk in self.fs._cat_file(filename):
                yield chunk

    def _cleanup_mode(self, mode=None):
        """Actual cleanup action to take based on various options"""
        if self._script_path and not self._ran_job:
            return mode or self._opts['cleanup_on_failure']
        else:
            return mode or self._opts['cleanup']

    def _cleanup_cloud_tmp(self):
        """Cleanup any files/directories on cloud storage (e.g. S3) we created
        while running this job. Should be safe to run this at any time, or
        multiple times.
        """
        pass  # only EMR runner does this

    def _cleanup_hadoop_tmp(self):
        """Cleanup any files/directories on HDFS we created
        while running this job. Should be safe to run this at any time, or
        multiple times.
        """
        pass  # only Hadoop runner does this

    def _cleanup_local_tmp(self):
        """Cleanup any files/directories on the local machine we created while
        running this job. Should be safe to run this at any time, or multiple
        times.

        This particular function removes any local tmp directories
        added to the list self._local_tmp_dirs

        This won't remove output_dir if it's outside of our tmp dir.
        """
        if self._local_tmp_dir:
            log.info('Removing temp directory %s...' % self._local_tmp_dir)
            try:
                rmtree(self._local_tmp_dir)
            except OSError as e:
                log.exception(e)

        self._local_tmp_dir = None

    def _cleanup_cluster(self):
        """Terminate the cluster if there is one."""
        pass  # this only happens on EMR

    def _cleanup_logs(self):
        """Cleanup any log files that are created as a side-effect of the job.
        """
        pass  # this only happens on EMR

    def _cleanup_job(self):
        """Stop any jobs that we created that are still running."""
        pass  # currently disabled (see #1241)

    def cleanup(self, mode=None):
        """Clean up running jobs, temp files, and logs, subject to the
        *cleanup* option passed to the constructor.

        If you create your runner in a ``with`` block,
        :py:meth:`cleanup` will be called automatically::

            with mr_job.make_runner() as runner:
                ...

            # cleanup() called automatically here

        :param mode: override *cleanup* passed into the constructor. Should be
                     a list of strings from
                     :py:data:`~mrjob.options.CLEANUP_CHOICES`
        """
        mode = self._cleanup_mode(mode)

        def mode_has(*args):
            return any((choice in mode) for choice in args)

        if self._script_path and not self._ran_job:
            if mode_has('CLUSTER', 'ALL'):
                self._cleanup_cluster()

            if mode_has('JOB', 'ALL'):
                self._cleanup_job()

        if mode_has('ALL', 'TMP', 'CLOUD_TMP'):
            self._cleanup_cloud_tmp()

        if mode_has('ALL', 'TMP', 'HADOOP_TMP'):
            self._cleanup_hadoop_tmp()

        if mode_has('ALL', 'TMP', 'LOCAL_TMP'):
            self._cleanup_local_tmp()

        if mode_has('ALL', 'LOGS'):
            self._cleanup_logs()

        self._closed = True

    def counters(self):
        """Get counters associated with this run in this form::

            [{'group name': {'counter1': 1, 'counter2': 2}},
             {'group name': ...}]

        The list contains an entry for every step of the current job.
        """
        raise NotImplementedError

    ### hooks for the with statement ###

    def __enter__(self):
        """Don't do anything special at start of with block"""
        return self

    def __exit__(self, type, value, traceback):
        """Call self.cleanup() at end of with block."""
        self.cleanup()

    ### more runner information ###

    def get_opts(self):
        """Get options set for this runner, as a dict."""
        log.warning('get_opts() is deprecated and will be removed in v0.7.0')
        return copy.deepcopy(self._opts)

    def get_job_key(self):
        """Get the unique key for the job run by this runner.
        This has the format ``label.owner.date.time.microseconds``
        """
        return self._job_key

    def get_output_dir(self):
        """Find the directory containing the job output. If the job hasn't
        run yet, returns None"""
        if self._script_path and not self._ran_job:
            return None

        return self._output_dir

    ### other methods you need to implement in your subclass ###

    def get_hadoop_version(self):
        """Return the version number of the Hadoop environment as a string if
        Hadoop is being used or simulated. Return None if not applicable.

        :py:class:`~mrjob.emr.EMRJobRunner` infers this from the cluster.
        :py:class:`~mrjob.hadoop.HadoopJobRunner` gets this from
        ``hadoop version``. :py:class:`~mrjob.local.LocalMRJobRunner` has an
        additional `hadoop_version` option to specify which version it
        simulates.
        :py:class:`~mrjob.inline.InlineMRJobRunner` does not simulate Hadoop at
        all.
        """
        return None

    # you'll probably wan't to add your own __init__() and cleanup() as well

    def _run(self):
        """Run the job."""
        raise NotImplementedError

    ### internal utilities for implementing MRJobRunners ###

    def _get_local_tmp_dir(self):
        """Create a tmp directory on the local filesystem that will be
        cleaned up by self.cleanup()"""
        if not self._local_tmp_dir:
            tmp_dir = (self._opts['local_tmp_dir'] or
                       tempfile.gettempdir())

            path = os.path.join(tmp_dir, self._job_key)
            log.info('Creating temp directory %s' % path)
            if os.path.isdir(path):
                rmtree(path)
            os.makedirs(path)
            self._local_tmp_dir = path

        return self._local_tmp_dir

    def _make_unique_job_key(self, label=None, owner=None):
        """Come up with a useful unique ID for this job. Optionally,
        you can specify a custom label or owner (otherwise we use
        :py:meth:`_label` and :py:meth:`_owner`.

        We use this to choose the output directory, etc. for the job.
        """
        if label is None:
            label = self._label()

        if owner is None:
            owner = self._owner()

        now = datetime.datetime.utcnow()
        return '%s.%s.%s.%06d' % (
            label, owner,
            now.strftime('%Y%m%d.%H%M%S'), now.microsecond)

    def _label(self):
        """Return *label* opt, or if not set, the name of the file
        containing the MRJob, minus extension, or if none, ``'no_script'``"""
        if self._opts['label']:
            return self._opts['label']
        elif self._script_path:
            return os.path.basename(self._script_path).split('.')[0]
        else:
            return 'no_script'

    def _owner(self):
        """Return *owner* opt (which defaults to :py:func:`getpass.getuser`),
        or ``'no_user'`` if not set."""
        if self._opts['owner']:
            # owner opt defaults to getpass.getuser()
            return self._opts['owner']
        else:
            return 'no_user'

    def _get_steps(self):
        """Returns ``self._steps``.
        """
        # TODO: remove this
        return self._steps

    def _check_steps(self, steps):
        """Look at the step definition (*steps*). If it is not supported by
        the runner, raise :py:class:`NotImplementedError`. If it is not
        supported by mrjob, raise :py:class:`ValueError`.
        """
        if not self._STEP_TYPES:
            # use __class__.__name__ because only MRJobRunner would
            # trigger this
            raise NotImplementedError(
                '%s cannot run steps!' % self.__class__.__name__)

        for step_num, step in enumerate(steps):
            self._check_step(step, step_num)

    def _check_step(self, step, step_num):
        """Raise an exception if the given step is invalid
        (:py:class:`ValueError`) or not handled by this runner
        (:py:class:`NotImplementedError`).

        By default, we check that *step* has a support step type,
        only uses an input manifest if it's the first step, and that
        :py:attr:`_script_path` exists if necessary. You can re-define
        this in your subclass.
        """
        if step.get('type') not in self._STEP_TYPES:
            raise NotImplementedError(
                'step %d has type %r, but %s runner only supports:'
                ' %s' % (step_num, step.get('type'), self.alias,
                         ', '.join(sorted(self._STEP_TYPES))))

        if step.get('input_manifest') and step_num != 0:
            raise ValueError(
                'step %d may not take an input manifest (only'
                ' first step can' % step_num)

        # some step types assume a MRJob script
        if not self._script_path:
            if step['type'] == 'spark':
                raise ValueError(
                    "SparkStep (step %d) can't run without a MRJob script"
                    " (try SparkScriptStep instead)" % step_num)

            elif step['type'] == 'streaming':
                for mrc in ('mapper', 'combiner', 'reducer'):
                    if not step.get(mrc):
                        continue

                    substep = step[mrc]
                    if substep['type'] == 'script':
                        raise ValueError(
                            "%s (step %d) can't run without a MRJob"
                            " script" % (mrc, step_num))

    def _get_step(self, step_num):
        """Get a single step (calls :py:meth:`_get_steps`)."""
        return self._get_steps()[step_num]

    def _num_steps(self):
        """Get the number of steps (calls :py:meth:`get_steps`)."""
        return len(self._get_steps())

    def _uses_input_manifest(self):
        """Does the first step take an input manifest?"""
        return bool(self._get_step(0).get('input_manifest'))

    def _has_hadoop_streaming_steps(self):
        """Are any of our steps Hadoop Streaming steps?"""
        return any(step['type'] == 'streaming'
                   for step in self._get_steps())

    def _has_spark_steps(self):
        """Are any of our steps Spark steps? (e.g. spark, spark_jar,
        spark_script)

        Generally used to determine if we need to install Spark on a cluster.
        """
        return any(self._step_type_uses_spark(step['type'])
                   for step in self._get_steps())

    def _has_pyspark_steps(self):
        """Do any of our steps involve running Python on Spark?
        Includes spark and spark_script types, but not spark_jar.

        Generally used to tell if we need a Spark setup script.
        """
        return any(self._step_type_uses_pyspark(step['type'])
                   for step in self._get_steps())

    def _step_type_uses_spark(self, step_type):
        """Does this step run on Spark?

        (This is re-defined in the Spark runner to include
        streaming steps, and used by mrjob.logs.mixin)
        """
        return _is_spark_step_type(step_type)

    def _step_type_uses_pyspark(self, step_type):
        """Does this step involve running Python on Spark?

        (This is re-defined in the Spark runner to include
        streaming steps, and used by mrjob.logs.mixin)
        """
        return _is_pyspark_step_type(step_type)

    def _spark_master(self):
        return self._opts.get('spark_master') or 'local[*]'

    def _spark_deploy_mode(self):
        return self._opts.get('spark_deploy_mode') or 'client'

    def _spark_driver_has_own_wd(self):
        """Does the spark driver have a working directory different
        from the one *spark-submit* was run in?

        (Only true in cluster mode.)
        """
        return (self._spark_deploy_mode() == 'cluster' and
                self._spark_executors_have_own_wd())

    def _spark_executors_have_own_wd(self):
        """Do spark executors have a working directory different
        from the one *spark-submit* was run in?

        (True on everything but local.)
        """
        # note: local-cluster[...] master does in fact have working dirs
        return self._spark_master().split('[')[0] != 'local'

    def _emulate_archives_on_spark(self):
        """True if spark-submit's --archives doesn't work on the given Spark
        master, which means we'll need to emulate archives in setup scripts.
        """
        return self._spark_master() != 'yarn'

    def _args_for_task(self, step_num, mrc):
        return [
            '--step-num=%d' % step_num,
            '--%s' % mrc,
        ] + self._mr_job_extra_args()

    def _mr_job_extra_args(self, local=False):
        """Return arguments to add to every invocation of MRJob.

        :type local: boolean
        :param local: if this is True, use files' local paths rather than
            the path they'll have inside Hadoop streaming
        """
        result = []

        for extra_arg in self._extra_args:
            if isinstance(extra_arg, dict):
                if local:
                    result.append(extra_arg['path'])
                else:
                    result.append(self._working_dir_mgr.name(**extra_arg))
            else:
                result.append(extra_arg)

        return result

    def _spark_script_args(self, step_num, last_step_num=None):
        """A list of args to the spark script/jar/MRJob, used by
        _args_for_spark_step().

        *last_step_num* is only used by the Spark runner, where multiple
        streaming steps are run in a single Spark job."""
        step = self._get_step(step_num)

        if step['type'] == 'spark':
            # if on local[*] master, keep file upload args as-is (see #2031)
            local = not self._spark_executors_have_own_wd()

            args = (
                [
                    '--step-num=%d' % step_num,
                    '--spark',
                ] + self._mr_job_extra_args(local=local) + [
                    INPUT,
                    OUTPUT,
                ]
            )
        elif step['type'] in ('spark_jar', 'spark_script'):
            args = step['args']
        else:
            raise TypeError('Bad step type: %r' % step['type'])

        return self._interpolate_step_args(args, step_num)

    def _interpolate_step_args(self, args, step_num):
        """Replace :py:data:`~mrjob.step.INPUT` and
        :py:data:`~mrjob.step.OUTPUT` in arguments to a jar or Spark
        step.
        """
        result = []

        for arg in args:
            if arg == INPUT:
                result.append(
                    ','.join(self._step_input_uris(step_num)))

            elif arg == OUTPUT:
                result.append(
                    self._step_output_uri(step_num))

            else:
                result.append(arg)

        return result

    def _dir_archive_path(self, dir_path):
        """Assign a path for the archive of *dir_path* but don't
        actually create anything."""
        if dir_path not in self._dir_to_archive_path:
            # we can check local paths now
            if not (is_uri(dir_path) or os.path.isdir(dir_path)):
                raise OSError('%s is not a directory!' % dir_path)

            name = name_uniquely(
                dir_path, names_taken=self._dir_archive_names_taken)
            self._dir_archive_names_taken.add(name)

            self._dir_to_archive_path[dir_path] = os.path.join(
                self._get_local_tmp_dir(), 'archives', name + '.tar.gz')

        return self._dir_to_archive_path[dir_path]

    def _create_dir_archives(self):
        """Call this to create all dir archives"""
        for dir_path in sorted(set(self._dir_to_archive_path)):
            self._create_dir_archive(dir_path)

    def _create_dir_archive(self, dir_path):
        """Helper for :py:meth:`archive_dir`"""
        if not self.fs.exists(dir_path):
            raise OSError('%s does not exist')

        tar_gz_path = self._dir_archive_path(dir_path)

        if tar_gz_path in self._dir_archives_created:
            return  # already created

        if not os.path.isdir(os.path.dirname(tar_gz_path)):
            os.makedirs(os.path.dirname(tar_gz_path))

        # for remote files
        tmp_download_path = os.path.join(
            self._get_local_tmp_dir(), 'tmp-download')

        log.info('Archiving %s -> %s' % (dir_path, tar_gz_path))

        with tarfile.open(tar_gz_path, mode='w:gz') as tar_gz:
            for path in self.fs.ls(dir_path):
                # fs.ls() only lists files
                if path == dir_path:
                    raise OSError('%s is a file, not a directory!' % dir_path)

                # TODO: do we need this?
                if os.path.realpath(path) == os.path.realpath(tar_gz_path):
                    raise OSError(
                        'attempted to archive %s into itself!' % tar_gz_path)

                if is_uri(path):
                    path_in_tar_gz = path[len(dir_path):].lstrip('/')

                    log.info('  downloading %s -> %s' % (
                        path, tmp_download_path))
                    with open(tmp_download_path, 'wb') as f:
                        for chunk in self.fs.cat(path):
                            f.write(chunk)
                    local_path = tmp_download_path
                else:
                    path_in_tar_gz = path[len(dir_path):].lstrip(os.sep)
                    local_path = path

                log.debug('  adding %s to %s' % (path, tar_gz_path))
                tar_gz.add(local_path, path_in_tar_gz, recursive=False)

        self._dir_archives_created.add(tar_gz_path)

    def _bootstrap_mrjob(self):
        """Should we bootstrap mrjob?"""
        if self._opts['bootstrap_mrjob'] is None:
            return True
        else:
            return bool(self._opts['bootstrap_mrjob'])

    def _get_input_paths(self):
        """Get the paths to input files, dumping STDIN to a local
        file if need be."""
        if self._input_manifest_path:
            return [self._input_manifest_path]

        if '-' in self._input_paths:
            if self._stdin_path is None:
                # prompt user, so they don't think the process has stalled
                log.info('reading from STDIN')

                stdin_path = os.path.join(self._get_local_tmp_dir(), 'STDIN')
                log.debug('dumping stdin to local file %s' % stdin_path)
                with open(stdin_path, 'wb') as stdin_file:
                    for line in self._stdin:
                        # catch missing newlines (often happens with test data)
                        if not line.endswith(b'\n'):
                            line += b'\n'
                        stdin_file.write(line)

                self._stdin_path = stdin_path

        return [self._stdin_path if p == '-' else p for p in self._input_paths]

    def _create_input_manifest_if_needed(self):
        """Create a file with a list of URIs of input files."""
        if self._input_manifest_path or not self._uses_input_manifest():
            return

        uris = []

        log.info('finding input files to add to manifest...')

        for path in self._get_input_paths():
            log.debug('  in %s' % path)
            if is_uri(path):
                # URIs might be globs
                for uri in self.fs.ls(path):
                    uris.append(uri)
            else:
                # local paths are expected to be single files
                # (shell would resolve globs)
                if self._upload_mgr:
                    uris.append(self._upload_mgr.uri(path))
                else:
                    # just make sure job can find files from its working dir
                    uris.append(os.path.abspath(path))

        log.info('found %d input files' % len(uris))

        path = os.path.join(self._get_local_tmp_dir(), 'input-manifest.txt')
        self._write_script(uris, path, 'input manifest')

        self._input_manifest_path = path
        if self._upload_mgr:
            self._upload_mgr.add(self._input_manifest_path)

    def _check_input_paths(self):
        """Check that input exists prior to running the job, if the
        `check_input_paths` option is true."""
        if not self._opts['check_input_paths']:
            return

        for path in self._input_paths:
            self._check_input_path(path)

    def _check_input_path(self, path):
        """Raise :py:class:`IOError` if the given input does not exist or
        is otherwise invalid. Override this to provide custom check
        behavior."""
        if path == '-':
            return  # STDIN always exists

        if not self.fs.can_handle_path(path):
            return  # no way to check (e.g. non-S3 URIs on EMR)

        if not self.fs.exists(path):
            raise IOError(
                'Input path %s does not exist!' % (path,))

    def _add_input_files_for_upload(self):
        """If there is an upload manager, add input files to it."""
        if self._upload_mgr:
            for path in self._get_input_paths():
                self._upload_mgr.add(path)

    def _upload_local_files(self):
        self._copy_files_to_wd_mirror()

        if self._upload_mgr:
            self.fs.mkdir(self._upload_mgr.prefix)

            log.info('Copying other local files to %s' %
                     self._upload_mgr.prefix)
            for src_path, uri in self._upload_mgr.path_to_uri().items():
                log.debug('  %s -> %s' % (src_path, uri))
                self.fs.put(src_path, uri)

    def _wd_mirror(self):
        """A directory to upload files belonging to
        :py:attr:`_working_dir_mgr`. This will be a subdir of
        ``self._upload_mgr.prefix`, if it exists, and otherwise will
        be ``None``."""
        if self._upload_mgr and is_uri(self._upload_mgr.prefix):
            return posixpath.join(self._upload_mgr.prefix, 'wd')
        elif (self._has_spark_steps() and self._spark_executors_have_own_wd()):
            return os.path.join(self._get_local_tmp_dir(), 'wd-mirror')
        else:
            return None

    def _wd_filenames_must_match(self):
        """When we tell Hadoop/Spark to put files in the working directory,
        must they have the same names as the files in the working dir?

        This basically only happens with Spark on non-YARN masters. YARN/Hadoop
        allows you to specify a name for each file (``path#name_in_wd``).
        """
        return self._has_spark_steps() and self._spark_master() != 'yarn'

    def _dest_in_wd_mirror(self, path, name):
        """Return the URI of where to upload *path* so it can appear in the
        working dir as *name*, or ``None`` if it doesn't need to be uploaded.
        """
        dest_dir = self._wd_mirror()
        if not dest_dir:
            return None

        # the only reason to re-upload a URI is if it has the wrong name
        #
        # similarly, the only point of a local working dir mirror is
        # to rename things
        if (is_uri(path) or not is_uri(dest_dir)) and (
                posixpath.basename(path) == name or
                not self._wd_filenames_must_match()):
            return None

        return posixpath.join(dest_dir, name)

    def _copy_file_to_wd_mirror(self, path, name):
        """Upload/copy *path* to the appropriate place in the working dir
        mirror, if necessary.

        We don't track whether something has already been uploaded.
        """
        dest = self._dest_in_wd_mirror(path, name)
        if not dest:
            return

        if is_uri(path):
            # file is visible to non-YARN Spark, but has the wrong name, so
            # download and re-upload it
            wd_tmp = os.path.join(self._get_local_tmp_dir(), 'wd-mirror')
            self.fs.mkdir(wd_tmp)

            tmp_path = os.path.join(wd_tmp, name)

            log.debug('  %s <- %s' % (tmp_path, path))
            try:
                with open(tmp_path, 'wb') as tmp_f:
                    for chunk in self.fs.cat(path):
                        tmp_f.write(chunk)

                log.debug('  %s -> %s' % (tmp_path, dest))
                self.fs.put(tmp_path, dest)
            finally:
                os.remove(tmp_path)
        else:
            # upload it
            log.debug('  %s -> %s' % (path, dest))
            self.fs.put(path, dest)

    def _copy_files_to_wd_mirror(self):
        """Upload working dir files to the working dir mirror, if necessary.

        This does not handle archives, which we always rename with
        hash paths anyhow (see #2059).
        """
        wd_mirror = self._wd_mirror()
        if not wd_mirror:
            return

        self.fs.mkdir(wd_mirror)

        log.info('%s working dir files to %s...' %
                 ('uploading' if is_uri(wd_mirror) else 'copying', wd_mirror))

        for name, path in sorted(
                self._working_dir_mgr.name_to_path('file').items()):
            self._copy_file_to_wd_mirror(path, name)

        for name, path in sorted(
                self._working_dir_mgr.name_to_path('archive_file').items()):
            self._copy_file_to_wd_mirror(path, name)

    def _upload_part_size(self):
        """Part size for uploads, in bytes, or ``None``,
        from :mrjob-opt:`cloud_part_size_mb`"""
        if self._opts.get('cloud_part_size_mb'):
            return int(self._opts['cloud_part_size_mb'] * 1024 * 1024)
        else:
            return None

    def _intermediate_output_dir(self, step_num, local=False):
        """A directory for intermediate output for the given step number."""
        join = os.path.join if local else posixpath.join

        return join(
            self._step_output_dir or self._default_step_output_dir(),
            '%04d' % step_num)

    def _default_step_output_dir(self):
        """Where to put output for steps other than the last one,
        if not specified by the *output_dir* constructor keyword.
        Usually you want this to be on HDFS (most efficient).

        Define this in your runner subclass.
        """
        raise NotImplementedError

    def _step_input_uris(self, step_num):
        """A list of URIs to use as input for the given step. For all
        except the first step, this list will have a single item (a
        directory)."""
        if step_num == 0:
            return [self._upload_mgr.uri(path) if self._upload_mgr
                    else to_uri(path)
                    for path in self._get_input_paths()]
        else:
            return [to_uri(self._intermediate_output_dir(step_num - 1))]

    def _step_output_uri(self, step_num):
        """URI to use as output for the given step. This is either an
        intermediate dir (see :py:meth:`intermediate_output_uri`) or
        ``self._output_dir`` for the final step."""
        if step_num == len(self._get_steps()) - 1:
            return to_uri(self._output_dir)
        else:
            return to_uri(self._intermediate_output_dir(step_num))

    def _cmdenv(self):
        """Return a copy of ``self._opts['cmdenv']``. This exists so we
        can instrument cmdenv in runner subclasses."""
        return dict(self._opts['cmdenv'])

    def _jobconf_for_step(self, step_num):
        """Get the jobconf dictionary, optionally including step-specific
        jobconf info.

        Also translate jobconfs to the current Hadoop version, if necessary.
        """

        step = self._get_step(step_num)

        # _sort_values_jobconf() isn't relevant to Spark,
        # but it doesn't do any harm either

        jobconf = combine_jobconfs(self._sort_values_jobconf(),
                                   self._opts['jobconf'],
                                   step.get('jobconf'))

        # if user is using the wrong jobconfs, add in the correct ones
        # and log a warning
        hadoop_version = self.get_hadoop_version()
        if hadoop_version:
            jobconf = translate_jobconf_dict(jobconf, hadoop_version)

        return jobconf

    def _sort_values_jobconf(self):
        """Jobconf dictionary to enable sorting by value.
        """
        if not self._sort_values:
            return {}

        # translate _SORT_VALUES_JOBCONF to the correct Hadoop version,
        # without logging a warning
        hadoop_version = self.get_hadoop_version()

        jobconf = {}
        for k, v in _SORT_VALUES_JOBCONF.items():
            if hadoop_version:
                jobconf[translate_jobconf(k, hadoop_version)] = v
            else:
                for j in translate_jobconf_for_all_versions(k):
                    jobconf[j] = v

        return jobconf

    def _sort_values_partitioner(self):
        """Partitioner to use with *sort_values* keyword to the constructor."""
        if self._sort_values:
            return _SORT_VALUES_PARTITIONER
        else:
            return None

    def _upload_args(self):
        # just upload every file and archive in the working dir manager
        return self._upload_args_helper('-files', None, '-archives', None)

    def _upload_args_helper(
            self, files_opt_str, files, archives_opt_str, archives,
            always_use_hash=True, emulate_archives=False):
        args = []

        file_hash_paths = list(
            self._file_arg_hash_paths(files,
                                      always_use_hash=always_use_hash))

        # if emulating --archives, upload archives with files (we'll unpack
        # them later with a setup script)
        if emulate_archives:
            file_hash_paths.extend(
                self._file_archive_hash_paths(archives))

        # --files ...
        if file_hash_paths:
            args.append(files_opt_str)
            args.append(','.join(file_hash_paths))

        if not emulate_archives:
            archive_hash_paths = list(self._archive_arg_hash_paths(archives))

            # --archives ...
            if archive_hash_paths:
                args.append(archives_opt_str)
                args.append(','.join(archive_hash_paths))

        return args

    def _file_arg_hash_paths(self, named_paths=None, always_use_hash=True):
        """Helper function for the *upload_args methods. The names of all
        arguments to ``-files`` (or ``--files`` on Spark).

        If *always_use_hash* is false, only use ``path#name`` syntax
        when the name is different.
        """
        if named_paths is None:
            # just return every file managed by _working_dir_mgr
            named_paths = sorted(
                self._working_dir_mgr.name_to_path('file').items())

        for name, path in named_paths:
            if not name:
                name = self._working_dir_mgr.name('file', path)

            uri = self._dest_in_wd_mirror(path, name) or path

            if not always_use_hash and _basename(uri) == name:
                yield uri
            else:
                yield '%s#%s' % (uri, name)

    def _file_archive_hash_paths(self, named_paths=None):
        """Helper function for the *upload_args methods. The names of
        archives to pass to the ``--files`` switch of ``spark-submit``,
        since we can't use ``--archives``.

        The names in *named_paths* should be of the archive destination
        (the 'archive' type in WorkingDirManager)
        not of the filename we're going to copy the archive to before
        unpacking it into its destination (the 'archive_file' type).
        """
        if named_paths is None:
            named_paths = sorted(
                self._working_dir_mgr.name_to_path('archive').items())

        for name, path in named_paths:
            if not name:
                name = self._working_dir_mgr.name('archive', path)

            archive_file_name = self._working_dir_mgr.name(
                'archive_file', path)

            uri = self._dest_in_wd_mirror(path, archive_file_name) or path

            yield uri

    def _archive_arg_hash_paths(self, named_paths=None):
        """Helper function for the *upload_args methods. The names of all
        arguments to ``-archives`` (or ``--archives`` on Spark).
        """
        # we always use path#name syntax, even on Spark, because unlike
        # with --files, Spark will either accept that syntax with --archives
        # (if we're on YARN) or ignore --archives completely (if we're on
        # any other Spark master)
        if named_paths is None:
            # just return every archive managed by _working_dir_mgr
            named_paths = sorted(
                self._working_dir_mgr.name_to_path('archive').items())

        for name, path in named_paths:
            if not name:
                name = self._working_dir_mgr.name('archive', path)

            # archives are uploaded to the working dir mirror by the
            # name of the original archive file, not the dir it unpacks into
            archive_file_name = self._working_dir_mgr.name(
                'archive_file', path)

            uri = self._dest_in_wd_mirror(path, archive_file_name) or path

            yield '%s#%s' % (uri, name)

    def _write_script(self, lines, path, description):
        """Write text of a setup script, input manifest, etc. to the given
        file.

        By default, this writes binary data. Redefine :py:meth:`write_lines`
        to use other line endings.

        :param lines: a list of lines as ``str``
        :param path: path of file to write to
        :param description: what we're writing to, for debug messages
        """
        log.debug('Writing %s to %s:' % (description, path))
        for line in lines:
            log.debug('  ' + line)

        self._write_script_lines(lines, path)

    def _write_script_lines(self, lines, path):
        """Write text to the given file. By default, this writes
        binary data, but can be redefined to use local line endings."""
        with open(path, 'wb') as f:
            for line in lines:
                f.write((line + '\n').encode('utf-8'))


def _fix_env(env):
    """Convert environment dictionary to strings (Python 2.7 on Windows
    doesn't allow unicode)."""
    def _to_str(s):
        if isinstance(s, string_types) and not isinstance(s, str):
            return s.encode('utf_8')
        else:
            return s

    return dict((_to_str(k), _to_str(v)) for k, v in env.items())


def _blank_out_conflicting_opts(opt_list, opt_names, conflicting_opts=None):
    """Utility for :py:meth:`MRJobRunner._combine_opts()`: if multiple
    configs specify conflicting opts, blank them out in all but the
    last config (so, for example, the command line beats the config file).

    This returns a copy of *opt_list*
    """
    conflicting_opts = set(conflicting_opts or ()) | set(opt_names)

    # copy opt_list so we can modify it
    opt_list = [dict(opts) for opts in opt_list]

    # blank out region/zone before the last config where they are set
    blank_out = False
    for opts in reversed(opt_list):
        if blank_out:
            for opt_name in opt_names:
                opts[opt_name] = None
        elif any(opts.get(opt_name) is not None
                 for opt_name in conflicting_opts):
            blank_out = True

    return opt_list


def _runner_class(alias):
    """Get the runner subclass corresponding to the given alias
    (importing code only as needed)."""
    if alias == 'dataproc':
        from mrjob.dataproc import DataprocJobRunner
        return DataprocJobRunner

    elif alias == 'emr':
        from mrjob.emr import EMRJobRunner
        return EMRJobRunner

    elif alias == 'hadoop':
        from mrjob.hadoop import HadoopJobRunner
        return HadoopJobRunner

    elif alias == 'inline':
        from mrjob.inline import InlineMRJobRunner
        return InlineMRJobRunner

    elif alias == 'local':
        from mrjob.local import LocalMRJobRunner
        return LocalMRJobRunner

    elif alias == 'spark':
        from mrjob.spark.runner import SparkMRJobRunner
        return SparkMRJobRunner

    else:
        raise ValueError('bad runner alias: %s' % alias)


def _basename(path_or_uri):
    if is_uri(path_or_uri):
        return posixpath.basename(path_or_uri)
    else:
        return os.path.basename(path_or_uri)
