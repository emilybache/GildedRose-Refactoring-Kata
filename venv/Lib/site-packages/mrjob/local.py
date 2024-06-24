# -*- coding: utf-8 -*-
# Copyright 2009-2013 Yelp and Contributors
# Copyright 2015-2017 Yelp
# Copyright 2018 Yelp and Contributors
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
"""Run an MRJob locally by forking off a bunch of processes and piping
them together. Useful for testing, not terrible for running medium-sized
jobs on all CPUs."""
import logging
import math
import os
import platform
from functools import partial
from multiprocessing import Pool
from subprocess import CalledProcessError
from subprocess import check_call

try:
    import pty
    pty  # quiet "redefinition of unused ..." warning from pyflakes
except ImportError:
    pty = None

from mrjob.bin import MRJobBinRunner
from mrjob.logs.errors import _log_probable_cause_of_failure
from mrjob.logs.errors import _pick_error
from mrjob.logs.step import _log_log4j_record
from mrjob.logs.task import _parse_task_stderr
from mrjob.py2 import string_types
from mrjob.sim import SimMRJobRunner
from mrjob.sim import _sort_lines_in_memory
from mrjob.step import StepFailedException
from mrjob.util import cmd_line

log = logging.getLogger(__name__)


_DEFAULT_EXECUTOR_MEMORY = '1g'


class _TaskFailedException(StepFailedException):
    """Extension of :py:class:`~mrjob.step.StepFailedException` that
    blames one particular task."""
    _FIELDS = StepFailedException._FIELDS + ('task_type', 'task_num')

    def __init__(
            self, reason=None, step_num=None, num_steps=None, step_desc=None,
            task_type=None, task_num=None):
        super(_TaskFailedException, self).__init__(
            reason=reason, step_num=step_num,
            num_steps=num_steps, step_desc=step_desc)

        self.task_type = task_type
        self.task_num = task_num


class LocalMRJobRunner(SimMRJobRunner, MRJobBinRunner):
    """Runs an :py:class:`~mrjob.job.MRJob` locally, for testing purposes.
    Invoked when you run your job with ``-r local``.

    Unlike :py:class:`~mrjob.job.InlineMRJobRunner`, this actually spawns
    multiple subprocesses for each task.

    It's rare to need to instantiate this class directly (see
    :py:meth:`~LocalMRJobRunner.__init__` for details).

    .. versionadded:: 0.6.8

       can run Spark steps as well, on the ``local-cluster`` Spark master.
    """
    alias = 'local'

    OPT_NAMES = SimMRJobRunner.OPT_NAMES | MRJobBinRunner.OPT_NAMES | {
        'sort_bin',
    }

    _STEP_TYPES = (
        SimMRJobRunner._STEP_TYPES | {'spark_jar', 'spark_script'})

    def __init__(self, **kwargs):
        """Arguments to this constructor may also appear in :file:`mrjob.conf`
        under ``runners/local``.

        :py:class:`~mrjob.local.LocalMRJobRunner`'s constructor takes the
        same keyword args as
        :py:class:`~mrjob.runner.MRJobRunner`. However, please note:

        * *cmdenv* is combined with :py:func:`~mrjob.conf.combine_local_envs`
        * *python_bin* defaults to ``sys.executable`` (the current python
          interpreter)
        * *hadoop_input_format*, *hadoop_output_format*,
          and *partitioner* are ignored because they
          require Java. If you need to test these, consider starting up a
          standalone Hadoop instance and running your job with ``-r hadoop``.
        """
        super(LocalMRJobRunner, self).__init__(**kwargs)

    def _invoke_task_func(self, task_type, step_num, task_num):
        args = self._substep_args(step_num, task_type)
        num_steps = self._num_steps()

        # stdin, stdout, stderr, wd, and env will be passed in later
        return partial(
            _invoke_task_in_subprocess,
            task_type, step_num, task_num,
            args, num_steps)

    def _run_step_on_spark(self, step, step_num):
        if self._opts['upload_archives']:
            log.warning('Spark master %r will probably ignore archives' %
                        self._spark_master())

        spark_submit_args = self._args_for_spark_step(step_num)

        env = dict(os.environ)
        env.update(self._spark_cmdenv(step_num))

        returncode, step_interpretation = self._run_spark_submit(
            spark_submit_args, env, record_callback=_log_log4j_record)

        if returncode:
            error = _pick_error(dict(step=step_interpretation))
            if error:
                _log_probable_cause_of_failure(log, error)

            reason = str(CalledProcessError(returncode, spark_submit_args))
            raise StepFailedException(
                reason=reason, step_num=step_num,
                num_steps=self._num_steps())

    def _run_multiple(self, funcs, num_processes=None):
        """Use multiprocessing to run in parallel."""
        pool = Pool(processes=self._opts['num_cores'])

        try:
            results = [
                pool.apply_async(partial(_pickle_safe, func))
                for func in funcs
            ]

            for result in results:
                result.get()

        # make sure that the pool (and its file descriptors, etc.)
        # don't stay open. This doesn't matter much for individual jobs,
        # but it makes our automated tasks run out of file descriptors.

            pool.close()
        except:
            # if there's an error in one task, terminate all others
            pool.terminate()
            raise
        finally:
            pool.join()

    def _log_cause_of_error(self, ex):
        if not isinstance(ex, _TaskFailedException):
            # if something went wrong inside mrjob, the stacktrace
            # will bubble up to the top level
            return

        # not using LogInterpretationMixin because it would be overkill

        if not self._opts['read_logs']:
            return

        input_path = self._task_input_path(
            ex.task_type, ex.step_num, ex.task_num)
        stderr_path = self._task_stderr_path(
            ex.task_type, ex.step_num, ex.task_num)

        if self.fs.exists(stderr_path):  # it should, but just to be safe
            # log-parsing code expects "str", not bytes; open in text mode
            with open(stderr_path) as stderr:
                task_error = _parse_task_stderr(stderr)
                if task_error:
                    task_error['path'] = stderr_path
                    error = dict(
                        split=dict(path=input_path),
                        task_error=task_error)
                    _log_probable_cause_of_failure(log, error)
                    return

        # fallback if we can't find the error (e.g. the job does something
        # weird to stderr or stack traces)
        log.error('Error while reading from %s:\n' % input_path)

    def _default_python_bin(self, local=False):
        """Always return *sys.executable*, if defined"""
        return super(LocalMRJobRunner, self)._default_python_bin(
            local=True)

    def _sort_input_func(self):
        """Try sorting with the :command:`sort` binary before falling
        back to in-memory sort."""
        if platform.system() == 'Windows':  # we assume Unix sort
            return super(LocalMRJobRunner, self)._sort_input_func()
        else:
            return partial(
                _sort_lines_with_sort_bin,
                sort_bin=self._sort_bin(),
                tmp_dir=self._get_local_tmp_dir())

    def _sort_bin(self):
        """The binary to use to sort input.

        (On Windows, we go straight to sorting in memory.)
        """
        if self._opts['sort_bin']:
            return self._opts['sort_bin']
        elif self._sort_values:
            return ['sort']
        else:
            # only sort on the reducer key (see #660)
            return ['sort', '-t', '\t', '-k', '1,1', '-s']

    # Spark steps

    # TODO: _spark_master() should probably take step_num, to allow for
    # step-specific jobconf
    def _spark_master(self):
        """Use the local-cluster master, which simulates a Spark cluster."""
        # figure out the required parameters to local-cluster
        num_executors = self._num_cores()

        # for now assigning one core per executor, so we don't have to worry
        # about a number of cores that's not evenly divisible
        cores_per_executor = 1

        executor_mem_bytes = _to_num_bytes(
            self._opts['jobconf'].get('spark.executor.memory') or
            _DEFAULT_EXECUTOR_MEMORY)
        executor_mem_mb = math.ceil(executor_mem_bytes / 1024.0 / 1024.0)

        return 'local-cluster[%d,%d,%d]' % (
            num_executors, cores_per_executor, executor_mem_mb)


def _to_num_bytes(java_mem_str):
    if isinstance(java_mem_str, string_types):
        for i, magnitude in enumerate(('k', 'm', 'g', 't'), start=1):
            if java_mem_str.lower().endswith(magnitude):
                return int(java_mem_str[:-1]) * 1024 ** i

    return int(java_mem_str)


# pickle utilities, to protect multiprocessing from itself

def _invoke_task_in_subprocess(
        task_type, step_num, task_num,
        args, num_steps,
        stdin, stdout, stderr, wd, env):
    """A pickleable function that invokes a task in a subprocess."""
    log.debug('> %s' % cmd_line(args))

    try:
        check_call(args, stdin=stdin, stdout=stdout, stderr=stderr,
                   cwd=wd, env=env)
    except Exception as ex:
        raise _TaskFailedException(
            reason=str(ex),
            step_num=step_num,
            num_steps=num_steps,
            task_type=task_type,
            task_num=task_num,
        )


def _pickle_safe(func):
    """Call no-args function *func*, returning *None* and ensuring
    that any exception raised is pickleable."""
    try:
        func()  # always return None
    except _TaskFailedException:
        raise  # we know these are pickleable
    except Exception as ex:
        raise Exception(repr(ex))  # we know this is pickleable


# other utilities

def _sort_lines_with_sort_bin(input_paths, output_path, sort_bin,
                              sort_values=False, tmp_dir=None):
    """Sort lines the given *input_paths* into *output_path*,
    using *sort_bin*. If there is a problem, fall back to in-memory sort.

    This is a helper for :py:meth:`LocalMRJobRunner._sort_input_func`.

    *tmp_dir* determines the value of :envvar:`$TMP` and :envvar:`$TMPDIR`
    that *sort_bin* sees.
    """
    if input_paths:
        env = os.environ.copy()

        # ignore locale when sorting
        env['LC_ALL'] = 'C'

        # Make sure that the tmp dir environment variables are changed if
        # the default is changed.
        env['TMP'] = tmp_dir
        env['TMPDIR'] = tmp_dir

        with open(output_path, 'wb') as output:
            args = sort_bin + list(input_paths)
            log.debug('> %s' % cmd_line(args))

            try:
                check_call(args, stdout=output, env=env)
                return
            except CalledProcessError:
                log.error(
                    '`%s` failed, falling back to in-memory sort' %
                    cmd_line(sort_bin))
            except OSError:
                log.error(
                    'no sort binary, falling back to in-memory sort')

    _sort_lines_in_memory(input_paths, output_path, sort_values=sort_values)
