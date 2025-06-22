# -*- coding: utf-8 -*-
# Copyright 2011 Matthew Tai and Yelp
# Copyright 2012-2016 Yelp and Contributors
# Copyright 2017-2018 Yelp
# Copyright 2019 Yelp
# Coypright 2020 Affirm, Inc.
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
"""Run an MRJob inline by running all mappers and reducers through the same
process. Useful for debugging."""
import logging
import os
import sys

from mrjob.job import MRJob
from mrjob.runner import _fix_env
from mrjob.sim import SimMRJobRunner
from mrjob.util import save_current_environment
from mrjob.util import save_cwd
from mrjob.util import save_sys_path
from mrjob.util import save_sys_std

log = logging.getLogger(__name__)


class InlineMRJobRunner(SimMRJobRunner):
    """Runs an :py:class:`~mrjob.job.MRJob` in the same process, so it's easy
    to attach a debugger.

    This is the default way to run jobs (we assume you'll spend some time
    debugging your job before you're ready to run it on EMR or Hadoop).

    Unlike other runners, ``InlineMRJobRunner``\\'s ``run()`` method
    raises the actual exception that caused a step to fail (rather than
    :py:class:`~mrjob.step.StepFailedException`).

    To more accurately simulate your environment prior to running on
    Hadoop/EMR, use ``-r local`` (see
    :py:class:`~mrjob.local.LocalMRJobRunner`).

    .. versionadded:: 0.6.8

       can run :py:class:`~mrjob.step.SparkStep`\\s via the
       :py:mod:`pyspark` library.
    """
    alias = 'inline'

    def __init__(self, mrjob_cls=None, **kwargs):
        """:py:class:`~mrjob.inline.InlineMRJobRunner` takes the same keyword
        args as :py:class:`~mrjob.runner.MRJobRunner`. However, please note
        that
        *hadoop_input_format*, *hadoop_output_format*, and *partitioner*
        are ignored
        because they require Java. If you need to test these, consider
        starting up a standalone Hadoop instance and running your job with
        ``-r hadoop``."""
        super(InlineMRJobRunner, self).__init__(**kwargs)
        # if we run python -m mrjob.job, mrjob_cls is __main__.MRJob
        # which is identical to (but not a subclass of) mrjob.job.MRJob
        #
        # the base MRJob still isn't runnable, but this yields a more
        # useful error about the step having no mappers or reducers
        if not (mrjob_cls is None or issubclass(mrjob_cls, MRJob) or
                mrjob_cls.__module__ == '__main__'):
            raise TypeError

        self._mrjob_cls = mrjob_cls

        # used to explain exceptions
        self._error_while_reading_from = None

        if self._opts['py_files']:
            log.warning("inline runner doesn't import py_files")

        if self._opts['setup']:
            log.warning("inline runner can't run setup commands")

    def _check_step(self, step, step_num):
        """Don't try to run steps that include commands."""
        super(InlineMRJobRunner, self)._check_step(step, step_num)

        if step['type'] == 'streaming':
            for mrc in ('mapper', 'combiner', 'reducer'):
                if step.get(mrc):
                    if 'command' in step[mrc] or 'pre_filter' in step[mrc]:
                        raise NotImplementedError(
                            "step %d's %s runs a command, but inline"
                            " runner does not support subprocesses (try"
                            " -r local)" % (step_num, mrc))

    def _invoke_task_func(self, task_type, step_num, task_num):
        """Just run tasks in the same process."""
        manifest = (step_num == 0 and task_type == 'mapper' and
                    self._uses_input_manifest())

        # Don't care about pickleability since this runs in the same process
        def invoke_task(stdin, stdout, stderr, wd, env):
            with save_current_environment(), save_cwd(), save_sys_path(), \
                    save_sys_std():
                # pretend we're running the script in the working dir
                os.environ.update(env)
                os.chdir(wd)
                sys.path = [os.getcwd()] + sys.path

                # pretend we've redirected stdin/stdout/stderr
                sys.stdin = stdin
                sys.stdout = stdout
                sys.stderr = stderr

                input_uri = None
                try:
                    args = self._args_for_task(step_num, task_type)

                    if manifest:
                        # read input path from stdin, add to args
                        line = stdin.readline().decode('utf_8')
                        input_uri = line.split('\t')[-1].rstrip()
                        # input_uri is an absolute path, can serve
                        # as path and uri both
                        args = list(args) + [input_uri, input_uri]

                    task = self._mrjob_cls(args)
                    task.execute()
                except:
                    # so users can figure out where the exception came from;
                    # see _log_cause_of_error(). we can't wrap the exception
                    # because then we lose the stacktrace (which is the whole
                    # point of the inline runner)

                    if input_uri:  # from manifest
                        self._error_while_reading_from = input_uri
                    else:
                        self._error_while_reading_from = self._task_input_path(
                            task_type, step_num, task_num)

                    raise

        return invoke_task

    def _run_step_on_spark(self, step, step_num):
        """Set up a fake working directory and environment, and call the Spark
        method."""
        # this is kind of a Spark-specific mash-up of _run_streaming_step()
        # (in sim.py) and _invoke_task_func(), above

        # don't create the output dir for the step; that's Spark's job

        # breaking the Spark step down into tasks is pyspark's job, so
        # we just have a single dummy task

        self.fs.mkdir(self._task_dir('spark', step_num, task_num=0))
        # could potentially parse this for cause of error
        stderr_path = self._task_stderr_path('spark', step_num, task_num=0)
        stdout_path = self._task_output_path('spark', step_num, task_num=0)

        self._create_dist_cache_dir(step_num)
        wd = self._setup_working_dir('spark', step_num, task_num=0)

        # use abspath() on input URIs before changing working dir
        task_args = self._spark_script_args(step_num)

        with open(stdout_path, 'wb') as stdout, \
                open(stderr_path, 'wb') as stderr:
            with save_current_environment(), save_cwd(), save_sys_path(), \
                    save_sys_std():
                os.environ.update(_fix_env(self._cmdenv()))
                os.chdir(wd)
                sys.path = [os.getcwd()] + sys.path

                # pretend we redirected stdout and stderr
                sys.stdout, sys.stderr = stdout, stderr

                task = self._mrjob_cls(task_args)
                task.execute()

    def _log_cause_of_error(self, ex):
        """Just tell what file we were reading from (since they'll see
        the stacktrace from the actual exception)"""
        if self._error_while_reading_from:
            log.error('\nError while reading from %s:\n' %
                      self._error_while_reading_from)

    def _spark_executors_have_own_wd(self):
        return True  # because we fake it

    def _spark_driver_has_own_wd(self):
        return True  # because we fake it

    def _wd_mirror(self):
        return None  # no need for this, we set up the working dir (Spark too)
