# Copyright 2009-2013 Yelp and Contributors
# Copyright 2015-2017 Yelp
# Copyright 2018 Yelp and Contributors
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
import itertools
import logging
import os
import shutil
import stat
import platform
from copy import deepcopy
from functools import partial
from multiprocessing import cpu_count
from os.path import dirname
from os.path import isdir
from os.path import join
from os.path import relpath
from shutil import copy2
from shutil import copytree

from mrjob.cat import decompress
from mrjob.cat import is_compressed
from mrjob.compat import translate_jobconf
from mrjob.compat import translate_jobconf_for_all_versions
from mrjob.conf import combine_dicts
from mrjob.conf import combine_local_envs
from mrjob.fs.local import _from_file_uri
from mrjob.logs.counters import _format_counters
from mrjob.parse import parse_mr_job_stderr
from mrjob.runner import MRJobRunner
from mrjob.runner import _fix_env
from mrjob.step import _is_spark_step_type
from mrjob.util import unarchive

log = logging.getLogger(__name__)


# This class defers execution to a lot of other functions because of local
# mode which uses :mod:`multiprocessing`, which relies on pickling.

class SimMRJobRunner(MRJobRunner):

    # keyword arguments that we ignore because they require real Hadoop.
    # We look directly at self._<kwarg_name> because they aren't in
    # self._opts
    _IGNORED_HADOOP_KWARGS = [
        'hadoop_input_format',
        'hadoop_output_format',
        'partitioner',
    ]

    # options that we ignore becaue they require real Hadoop.
    _IGNORED_HADOOP_OPTS = [
        'libjars',
    ]

    OPT_NAMES = MRJobRunner.OPT_NAMES | {
        'hadoop_version',
        'num_cores'
    }

    _STEP_TYPES = {'spark', 'streaming'}

    def __init__(self, **kwargs):
        super(SimMRJobRunner, self).__init__(**kwargs)

        self._counters = []

        # warn about ignored keyword arguments
        for key in self._IGNORED_HADOOP_KWARGS:
            value = kwargs.get(key)
            if value is not None:
                log.warning(
                    'ignoring %s keyword arg (requires real Hadoop): %r' %
                    (key, value))

        # TODO: libjars should just not be an option for local runners
        #
        # however, the job class can still set it; might want to handle
        # this in the job itself
        for ignored_opt in self._IGNORED_HADOOP_OPTS:
            value = self._opts.get(ignored_opt)
            if value:  # ignore [], the default value of libjars
                log.warning(
                    'ignoring %s option (requires real Hadoop): %r' %
                    (ignored_opt, value))

    def _opt_combiners(self):
        """Combine *cmdenv* with :py:func:`~mrjob.conf.combine_local_envs`"""
        return combine_dicts(
            super(SimMRJobRunner, self)._opt_combiners(),
            dict(cmdenv=combine_local_envs),
        )

    # re-implement these in your subclass

    def _invoke_task_func(self, task_type, step_num, task_num):
        """Return a function that runs the given
        mapper/reducer. This needs to be pickleable if tasks are going
        to be invoked through multiprocessing (e.g. local mode).

        The function takes the arguments *stdin*, *stdout*, *stderr*,
        *wd* (path of working directory), and *env* (environment dictionary).

        The job's filehandle, working dir (*wd*) and environment
        will be provided.
        """
        NotImplementedError

    def _run_multiple(self, funcs, num_processes=None):
        """Run multiple no-args functions, possibly in parallel using
        :py:mod:`multiprocessing` (if so all *funcs* must be pickleable).

        By default, we just call *funcs* one at a time
        """
        for func in funcs:
            func()

    def _log_cause_of_error(self, ex):
        """Log why the job failed."""
        pass

    def _run_step_on_spark(self, step, step_num):
        """Run a Step on Spark. Override this in your subclass. You can
        assume that setup wrapper scripts are created (if relevant)
        and that self._counters has a dictionary for that step already"""
        raise NotImplementedError

    # other implementation

    def _run(self):
        if not self._output_dir:
            self._output_dir = join(self._get_local_tmp_dir(), 'output')

        if hasattr(self, '_create_setup_wrapper_scripts'):  # inline doesn't
            self._create_setup_wrapper_scripts()

        # this does nothing in inline mode, since there's no _spark_master()
        self._copy_files_to_wd_mirror()

        # run mapper, combiner, sort, reducer for each step
        for step_num, step in enumerate(self._get_steps()):
            log.info('Running step %d of %d...' % (
                step_num + 1, self._num_steps()))

            self._counters.append({})

            self._run_step(step, step_num)

    def _run_step(self, step, step_num):
        """Run an individual step. You can assume that setup wrapper scripts
        are created and self._counters has a dictionary for that step already.
        """
        if _is_spark_step_type(step['type']):
            self._run_step_on_spark(step, step_num)
        else:
            self._run_streaming_step(step, step_num)

    def _run_streaming_step(self, step, step_num):
        """Run a Hadoop streaming step on simulated Hadoop."""
        try:
            self._create_dist_cache_dir(step_num)
            self.fs.mkdir(self._output_dir_for_step(step_num))

            map_splits = self._split_mapper_input(
                self._input_paths_for_step(step_num), step_num)

            self._run_mappers_and_combiners(step_num, map_splits)

            if 'reducer' in step:
                self._sort_reducer_input(step_num, len(map_splits))
                num_reducer_tasks = self._split_reducer_input(step_num)

                self._run_reducers(step_num, num_reducer_tasks)

            self._log_counters(step_num)

        except Exception as ex:
            self._log_counters(step_num)
            self._log_cause_of_error(ex)

            raise

    def _run_task_func(self, task_type, step_num, task_num, map_split=None):
        """Returns a no-args function that runs one mapper, reducer, or
         combiner.

        This sets up everything the task needs to run, then passes it off to
        :py:meth:`_invoke_task_func`.
        """
        input_path = self._task_input_path(task_type, step_num, task_num)
        stderr_path = self._task_stderr_path(task_type, step_num, task_num)
        output_path = self._task_output_path(task_type, step_num, task_num)
        wd = self._setup_working_dir(task_type, step_num, task_num)
        env = _fix_env(
            self._env_for_task(task_type, step_num, task_num, map_split))

        return partial(
            _run_task,
            self._invoke_task_func(task_type, step_num, task_num),
            task_type, step_num, task_num,
            input_path, output_path, stderr_path, wd, env)

    def _run_mappers_and_combiners(self, step_num, map_splits):
        try:
            self._run_multiple(
                self._run_mapper_and_combiner_func(
                    step_num, task_num, map_split)
                for task_num, map_split in enumerate(map_splits)
            )
        finally:
            self._parse_task_counters('mapper', step_num)
            self._parse_task_counters('combiner', step_num)

    def _parse_task_counters(self, task_type, step_num):
        """Parse all stderr files from the given task (if any)."""
        # don't disable if read_logs=False; parsing counters is
        # internal to Hadoop, not something that happens in log files
        stderr_paths = self.fs.ls(self._task_stderr_paths_glob(
            task_type, step_num))

        for stderr_path in stderr_paths:
            with open(stderr_path, 'rb') as stderr:
                parse_mr_job_stderr(stderr, counters=self._counters[step_num])

    def counters(self):
        return deepcopy(self._counters)

    def get_hadoop_version(self):
        return self._opts['hadoop_version']

    def _write_script_lines(self, lines, path):
        """Write text to the given file, using local line endings."""
        with open(path, 'w') as f:
            for line in lines:
                f.write(line + '\n')

    def _run_mapper_and_combiner_func(self, step_num, task_num, map_split):
        """Returns a no-args function that runs one mapper, plus the
        corresponding combiner if there is one."""
        step = self._get_step(step_num)

        mapper_input_path = self._task_input_path(
            'mapper', step_num, task_num)
        mapper_output_path = self._task_output_path(
            'mapper', step_num, task_num)

        run_mapper = None

        if 'mapper' in step:
            run_mapper = self._run_task_func(
                'mapper', step_num, task_num, map_split)

        sort_input = self._sort_input_func()

        combiner_input_path = None
        run_combiner = None
        # don't need combiner_output_path; *run_combiner* already knows it

        if 'combiner' in step:
            # create combiner dir
            self.fs.mkdir(self._task_dir('combiner', step_num, task_num))

            combiner_input_path = self._task_input_path(
                'combiner', step_num, task_num)
            run_combiner = self._run_task_func(
                'combiner', step_num, task_num, map_split)

        return partial(
            _run_mapper_and_combiner,
            run_mapper, sort_input, run_combiner,
            mapper_input_path, mapper_output_path, combiner_input_path)

    def _run_reducers(self, step_num, num_reducer_tasks):
        try:
            self._run_multiple(
                self._run_task_func('reducer', step_num, task_num)
                for task_num in range(num_reducer_tasks)
            )
        finally:
            self._parse_task_counters('reducer', step_num)

    def _create_dist_cache_dir(self, step_num):
        """Copy working directory files into a shared directory,
        simulating the way Hadoop's Distributed Cache works on nodes."""
        cache_dir = self._dist_cache_dir(step_num)

        log.debug('creating simulated Distributed Cache dir: %s' % cache_dir)
        self.fs.mkdir(cache_dir)

        for name, path in self._working_dir_mgr.name_to_path('file').items():
            path = _from_file_uri(path)  # might start with file://
            dest = self._path_in_dist_cache_dir(name, step_num)
            log.debug('copying %s -> %s' % (path, dest))
            shutil.copy(path, dest)
            _chmod_u_rx(dest)

        for name, path in self._working_dir_mgr.name_to_path(
                'archive').items():
            path = _from_file_uri(path)  # might start with file://
            dest = self._path_in_dist_cache_dir(name, step_num)

            log.debug('unarchiving %s -> %s' % (path, dest))
            unarchive(path, dest)
            _chmod_u_rx(dest, recursive=True)

    def _env_for_task(self, task_type, step_num, task_num, map_split=None):
        """Set up environment variables for a subprocess (mapper, etc.)

        This combines, in decreasing order of priority:

        * environment variables set by the **cmdenv** option
        * **jobconf** environment variables set by our job (e.g.
          ``mapreduce.task.ismap`)
        * environment variables from **jobconf** options, translated to
          whatever version of Hadoop we're emulating
        * the current environment
        * PYTHONPATH set to current working directory

        We use :py:func:`~mrjob.conf.combine_local_envs`, so ``PATH``
        environment variables are handled specially.
        """
        user_jobconf = self._jobconf_for_step(step_num)

        simulated_jobconf = self._simulate_jobconf_for_step(
            task_type, step_num, task_num, map_split)

        def to_env(jobconf):
            return dict((k.replace('.', '_'), str(v))
                        for k, v in jobconf.items())

        # keep the current environment because we need PATH to find binaries
        # and make PYTHONPATH work
        return combine_local_envs(os.environ,
                                  to_env(user_jobconf),
                                  to_env(simulated_jobconf),
                                  self._cmdenv())

    def _simulate_jobconf_for_step(
            self, task_type, step_num, task_num, map_split=None):
        j = {}

        # TODO: these are really poor imtations of Hadoop IDs. See #1254
        j['mapreduce.job.id'] = self._job_key
        j['mapreduce.task.id'] = 'task_%s_%s_%04d%d' % (
            self._job_key, task_type.lower(), step_num, task_num)
        j['mapreduce.task.attempt.id'] = 'attempt_%s_%s_%04d%d_0' % (
            self._job_key, task_type.lower(), step_num, task_num)

        j['mapreduce.task.ismap'] = str(task_type == 'mapper').lower()

        # TODO: is this the correct format?
        j['mapreduce.task.partition'] = str(task_num)

        j['mapreduce.task.output.dir'] = self._output_dir_for_step(step_num)

        working_dir = self._task_working_dir(task_type, step_num, task_num)
        j['mapreduce.job.local.dir'] = working_dir

        for x in ('archive', 'file'):
            named_paths = sorted(self._working_dir_mgr.name_to_path(x).items())

            # mapreduce.job.cache.archives
            # mapreduce.job.cache.files
            j['mapreduce.job.cache.%ss' % x] = ','.join(
                '%s#%s' % (path, name) for name, path in named_paths)

            # mapreduce.job.cache.local.archives
            # mapreduce.job.cache.local.files
            j['mapreduce.job.cache.local.%ss' % x] = ','.join(
                join(working_dir, name) for name, path in named_paths)

        if map_split:
            j['mapreduce.map.input.file'] = 'file://' + map_split['file']
            j['mapreduce.map.input.length'] = str(map_split['length'])
            j['mapreduce.map.input.start'] = str(map_split['start'])

        # translate to correct version

        # don't use translate_jobconf_dict(); that's meant to add keys
        # to user-supplied jobconf
        hadoop_version = self.get_hadoop_version()

        if hadoop_version:
            return {translate_jobconf(k, hadoop_version): v
                    for k, v in j.items()}
        else:
            return {tk: v for k, v in j.items()
                    for tk in translate_jobconf_for_all_versions(k)}

    def _num_cores(self):
        return self._opts['num_cores'] or cpu_count()

    def _num_mappers(self, step_num):
        # TODO: look up mapred.job.maps (convert to int) in _jobconf_for_step()
        return self._num_cores()

    def _num_reducers(self, step_num):
        # TODO: look up mapred.job.reduces in _jobconf_for_step()
        return self._num_cores()

    def _split_mapper_input(self, input_paths, step_num):
        """Take one or more input paths (which may be compressed) and split
        it to create the input files for the map tasks.

        Yields "splits", which are dictionaries with the following keys:

        input: path of input for one mapper
        file: path of original file
        start, length: chunk of original file in *input*

        Uncompressed files will not be split (even ``.bz2`` files);
        uncompressed files will be split as to to attempt to create
        twice as many input files as there are mappers.
        """
        input_paths = list(input_paths)
        manifest = (step_num == 0 and self._uses_input_manifest())

        # determine split size
        if manifest:
            split_size = 1  # one line per mapper
        else:
            split_size = self._pick_mapper_split_size(input_paths, step_num)

        # yield output fileobjs as needed
        split_fileobj_gen = self._yield_split_fileobjs('mapper', step_num)

        results = []

        for path in input_paths:
            with open(path, 'rb') as src:
                if is_compressed(path):
                    if manifest:
                        raise Exception('input manifest %s should not be'
                                        ' compressed!' % path)

                    # if file is compressed, uncompress it into a single split

                    # Hadoop tracks the compressed file's size
                    size = os.stat(path)[stat.ST_SIZE]

                    with next(split_fileobj_gen) as dest:
                        for chunk in decompress(src, path):
                            dest.write(chunk)

                    results.append(dict(
                        file=path,
                        start=0,
                        length=size,
                    ))
                else:
                    # otherwise, split into one or more input files
                    start = 0
                    length = 0

                    for lines in _split_records(src, split_size):
                        with next(split_fileobj_gen) as dest:
                            for line in lines:
                                # simulate NLinesInputFormat by prefixing
                                # each line with byte number
                                if manifest:
                                    i = start + length
                                    dest.write(('%d\t' % i).encode('ascii'))
                                dest.write(line)
                                length += len(line)

                        results.append(dict(
                            file=path,
                            start=start,
                            length=length,
                        ))

                        start += length
                        length = 0

        return results

    def _pick_mapper_split_size(self, input_paths, step_num):
        if not isinstance(input_paths, list):
            raise TypeError

        target_num_splits = self._num_mappers(step_num) * 2

        # decide on a split size to approximate target_num_splits
        num_compressed = 0
        uncompressed_bytes = 0

        for path in input_paths:
            if path.endswith('.gz') or path.endswith('.bz'):
                num_compressed += 1
            else:
                uncompressed_bytes += os.stat(path)[stat.ST_SIZE]

        return uncompressed_bytes // max(
            target_num_splits - num_compressed, 1)

    def _split_reducer_input(self, step_num):
        """Split a single, uncompressed file containing sorted input for the
        reducer into input files for each reducer task.

        Yield the paths of the reducer input files."""
        path = self._sorted_reducer_input_path(step_num)

        log.debug('splitting reducer input: %s' % path)

        size = os.stat(path)[stat.ST_SIZE]
        split_size = size // (self._num_reducers(step_num) * 2)

        # yield output fileobjs as needed
        split_fileobj_gen = self._yield_split_fileobjs('reducer', step_num)

        def reducer_key(line):
            return line.split(b'\t')[0]

        num_reducer_tasks = 0

        with open(path, 'rb') as src:
            for records in _split_records(src, split_size, reducer_key):
                with next(split_fileobj_gen) as dest:
                    for record in records:
                        dest.write(record)
                    num_reducer_tasks += 1

        return num_reducer_tasks

    def _yield_split_fileobjs(self, task_type, step_num):
        """Used to split input for the given mapper/reducer.

        Yields writeable fileobjs for input splits (check their *name*
        attribute to get the path)
        """
        for task_num in itertools.count():
            path = self._task_input_path(task_type, step_num, task_num)
            self.fs.mkdir(dirname(path))
            yield open(path, 'wb')

    def _setup_working_dir(self, task_type, step_num, task_num):
        wd = self._task_working_dir(task_type, step_num, task_num)
        self.fs.mkdir(wd)

        for type in ('archive', 'file'):
            for name, path in (
                    self._working_dir_mgr.name_to_path(type).items()):
                _symlink_or_copy(
                    self._path_in_dist_cache_dir(name, step_num),
                    join(wd, name))

        return wd

    def _last_task_type_in_step(self, step_num):
        step = self._get_step(step_num)

        if step.get('reducer'):
            return 'reducer'
        elif step.get('combiner'):
            return 'combiner'
        else:
            return 'mapper'

    # directory structure

    # cache/

    def _dist_cache_dir(self, step_num):
        return join(self._step_dir(step_num), 'cache')

    # cache/<name>

    def _path_in_dist_cache_dir(self, name, step_num):
        return join(self._dist_cache_dir(step_num), name)

    # step/<step_num>/

    def _step_dir(self, step_num):
        return join(self._get_local_tmp_dir(), 'step', '%03d' % step_num)

    def _input_paths_for_step(self, step_num):
        if step_num == 0:
            return [
                _from_file_uri(path)  # *path* could be a file:// URI
                for input_path_glob in self._get_input_paths()
                for path in self.fs.ls(input_path_glob)
            ]
        else:
            return self.fs.ls(
                join(self._output_dir_for_step(step_num - 1), 'part-*'))

    def _output_dir_for_step(self, step_num):
        if step_num == self._num_steps() - 1:
            return self._output_dir
        else:
            return self._intermediate_output_dir(step_num, local=True)

    def _default_step_output_dir(self):
        return join(self._get_local_tmp_dir(), 'step-output')

    # step/<step_num>/<task_type>/<task_num>/

    def _task_dir(self, task_type, step_num, task_num):
        return join(self._step_dir(step_num), task_type, '%05d' % task_num)

    def _task_input_path(self, task_type, step_num, task_num):
        return join(
            self._task_dir(task_type, step_num, task_num), 'input')

    def _task_stderr_path(self, task_type, step_num, task_num):
        return join(
            self._task_dir(task_type, step_num, task_num), 'stderr')

    def _task_stderr_paths_glob(self, task_type, step_num):
        return join(
            self._step_dir(step_num), task_type, '*', 'stderr')

    def _task_output_path(self, task_type, step_num, task_num):
        """Where to output data for the given task.

        Usually this is just a file named "output" in the task's directory,
        but if it's the last task type in the step (usually the reducer),
        it outputs directly to part-XXXXX files in the step's output directory.
        """
        if task_type == self._last_task_type_in_step(step_num):
            return join(
                self._output_dir_for_step(step_num), 'part-%05d' % task_num)
        else:
            return join(
                self._task_dir(task_type, step_num, task_num), 'output')

    # step/<step_num>/<task_type>/<task_num>/wd

    def _task_working_dir(self, task_type, step_num, task_num):
        return join(self._task_dir(task_type, step_num, task_num), 'wd')

    # need this specifically since there's a global sort of reducer input

    def _sorted_reducer_input_path(self, step_num):
        return join(self._step_dir(step_num), 'reducer', 'sorted-input')

    def _sort_input_func(self):
        """Returns a function that sorts lines from one or more input paths
        into a new file. Takes the arguments *input_path* and *output_path*.

        By default, sorts in memory, but you can override this to
        use the :command:`sort` binary, etc.
        """
        return partial(_sort_lines_in_memory, sort_values=self._sort_values)

    def _sort_reducer_input(self, step_num, num_map_tasks):
        step = self._get_step(step_num)

        output_path = self._sorted_reducer_input_path(step_num)
        self.fs.mkdir(dirname(output_path))

        prev_task_type = 'combiner' if step.get('combiner') else 'mapper'
        input_paths = [
            self._task_output_path(prev_task_type, step_num, task_num)
            for task_num in range(num_map_tasks)
        ]

        self._sort_input_func()(input_paths, output_path)

    def _log_counters(self, step_num):
        counters = self.counters()[step_num]
        if counters:
            log.info('\n%s\n' % _format_counters(counters))


def _chmod_u_rx(path, recursive=False):
    """make *path* user readable and executable. If *recursive* is true,
    make *path* and everything inside it executable."""
    if recursive:
        for dir_name, _, file_names in os.walk(path, followlinks=True):
            for file_name in file_names:
                _chmod_u_rx(join(dir_name, file_name))
    else:
        # only available on Unix, causes rmtree() to fail on Windows; see #1847
        if hasattr(os, 'chmod') and platform.system() != "Windows":
            os.chmod(path, stat.S_IRUSR | stat.S_IXUSR)


def _group_records_for_split(record_gen, split_size, reducer_key=None):
    """Helper for _split_records()."""
    split_num = 0
    bytes_in_split = 0

    last_key_value = None

    for record in record_gen:
        same_key = False

        if reducer_key:
            key_value = reducer_key(record)
            same_key = (key_value == last_key_value)
            last_key_value = key_value

        if bytes_in_split >= split_size and not same_key:
            split_num += 1
            bytes_in_split = 0

        yield split_num, record
        bytes_in_split += len(record)


def _run_mapper_and_combiner(
        run_mapper, sort_input, run_combiner,
        mapper_input_path, mapper_output_path, combiner_input_path):
    """Helper for :py:meth:`SimMRJobRunner._run_mapper_and_combiner_func`."""
    # we don't need *combiner_output_path* because *run_combiner* already
    # knows it

    if run_mapper:
        run_mapper()
    else:
        _symlink_or_copy(mapper_input_path, mapper_output_path)

    if run_combiner:
        sort_input([mapper_output_path], combiner_input_path)
        run_combiner()


def _run_task(invoke_task,
              task_type, step_num, task_num,
              input_path, output_path, stderr_path, wd, env):
    """Set up filehandles and call *invoke_task()*.

    Helper for :py:meth:`SimMRJobRunner._run_task_func`.
    """
    log.debug('running step %d, %s %d' % (step_num, task_type, task_num))

    with open(input_path, 'rb') as stdin, \
            open(output_path, 'wb') as stdout, \
            open(stderr_path, 'wb') as stderr:

        invoke_task(
            stdin, stdout, stderr, wd, env)


def _sort_lines_in_memory(input_paths, output_path, sort_values=False):
    """Sort lines from *input_paths* and output them into *output_path*.

    If *sort_values* is true, sort by the entire line; otherwise just sort
    by everything up to the first tab.
    """
    log.debug('sorting in memory: %s -> %s' %
              (', '.join(input_paths), output_path))
    lines = []

    for input_path in input_paths:
        with open(input_path, 'rb') as input:
            lines.extend(input)

    if sort_values:
        lines.sort()
    else:
        lines.sort(key=lambda line: line.split(b'\t')[0])

    with open(output_path, 'wb') as output:
        for line in lines:
            output.write(line)


def _split_records(record_gen, split_size, reducer_key=None):
    """Given a stream of records (bytestrings, usually lines), yield groups of
    records (as generators such that the total number of bytes in each group
    only barely exceeds *split_size*, and, if *reducer_key* is set, consecutive
    records with the same key will be in the same split."""
    grouped_record_gen = _group_records_for_split(
        record_gen, split_size, reducer_key)

    num_records = 0
    for group_id, grouped_records in itertools.groupby(
            grouped_record_gen, key=lambda gr: gr[0]):
        yield (record for _, record in grouped_records)
        num_records += 1

    if num_records == 0:
        # special case for empty files
        yield ()


def _symlink_or_copy(path, dest):
    """Symlink from *dest* to *path*, using relative paths if possible.

    If symlinks aren't available, copy path to dest instead.
    """
    if hasattr(os, 'symlink'):
        log.debug('creating symlink %s <- %s' % (path, dest))
        try:
            os.symlink(relpath(path, dirname(dest)), dest)
            return
        except OSError as ex:
            log.debug('  %s' % ex)

    log.debug('copying %s -> %s' % (dest, path))
    if isdir(path):
        copytree(path, dest)
    else:
        copy2(path, dest)
