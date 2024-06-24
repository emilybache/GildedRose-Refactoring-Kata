# -*- coding: utf-8 -*-
# Copyright 2015-2018 Yelp
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
"""Utility for handling IDs, especially sorting by recency."""


def _sort_by_recency(ds):
    """Sort the given list/sequence of dicts containing IDs so that the
    most recent ones come first (e.g. to find the best error, or the best
    log file to look for an error in).
    """
    return sorted(ds, key=_time_sort_key, reverse=True)


def _sort_for_spark(ds):
    """Sort the given list/sequence of dicts in forward order by task/container
    ID, with most recent attempts first. This is used for finding errors on
    Spark, see #2056.
    """
    return (
        sorted(
            sorted(
                sorted(
                    ds, key=_attempt_num, reverse=True),
                key=_container_num),
            key=_step_sort_key, reverse=True))


def _time_sort_key(d):
    """Sort key to sort the given dictionaries containing IDs roughly by time
    (earliest first).

    We consider higher attempt_nums "later" than higher task_nums (of the
    same step type) because fatal errors usually occur on the final
    attempt of a task.

    If we can, we convert (YARN) container IDs to attempt IDs. Unconverted
    container IDs are considered more "recent" than any task/attempt ID
    (these usually come from task logs).
    """
    # when parsing task syslogs on YARN, we may end up with
    # container_id and nothing else. container IDs match with job ID
    # but aren't directly comparable to task and attempt IDs

    # But if we couldn't parse the history file (for example because
    # we're using YARN on EMR and the only way to get it is SSHing in and
    # finding it on HDFS), we can use the container ID to infer the
    # job ID. After that, we just assume that errors with a container
    # ID must be better (they usually include the task error, after all),
    # so we treat them as more recent.

    # break ID like
    # {application,attempt,task,job}_201601081945_0005[_m[_000005[_0]]]
    # into its component parts
    #
    # in practice, errors don't have job or application ID attached to
    # them (and we're only sorting errors from the same job/application)
    return (
        _timestamp(d),
        _step_num(d),
        d.get('container_id') or '',
        _task_type(d),
        _attempt_num(d),
        _task_num(d),
    )


def _step_sort_key(d):
    """Sort by timestamp and step"""
    return (
        _timestamp(d),
        _step_num(d),
    )


def _id_part(id, i):
    """Get the ith part of some container ID, e.g.
    container_1567013493699_0003_01_000002. If *id* is None
    or too short, return ''"""
    parts = (id or '').split('_')

    if i >= len(parts):
        return ''
    else:
        return parts[i]


def _any_id(d):
    return (d.get('attempt_id') or
            d.get('task_id') or d.get('job_id') or
            d.get('application_id') or
            d.get('container_id'))


# TODO: account for "epoch" from resource manager restarts. See:
# https://hadoop.apache.org/docs/current/api/org/apache/hadoop/yarn/api/records/ContainerId.html  # noqa

def _timestamp(d):
    """cluster timestamp. the first number in any ID.

    For example, in container_1567013493699_0003_01_000002, it's
    1567013493699."""
    return _id_part(_any_id(d), 1)


def _step_num(d):
    """Number that indicates this is the nth task to be launched on the
    cluster. The second number in any ID.

    For example, in container_1567013493699_0003_01_000002, it's 0003.
    """
    return _id_part(_any_id(d), 2)


def _task_type(d):
    """In task or attempt IDs, whether this is a map or reduce
    (not used by Spark).

    For example, in
    attempt_201601081945_0005_m_000005_0, it's "m"
    """
    return _id_part(d.get('attempt_id') or d.get('task_id'), 3)


def _task_num(d):
    """In task or attempt IDs, a unique number for the individual
    task within the map or reduce phase.

    For example, in
    task_201601081945_0005_m_000005, it's 000005
    """
    return _id_part(d.get('attempt_id') or d.get('task_id'), 4)


def _container_num(d):
    """In container IDs, a unique number for the task assigned to the
    container. Could be a map, a reduce, or part of a Spark
    task; YARN doesn't care.

    For example, in container_1567013493699_0003_01_000002, it's 000002.
    """
    return _id_part(d.get('container_id'), 4)


def _attempt_num(d):
    """Which attempt this is for a particular task; typically tasks that
    fail are retried one or two times. This applies to both attempt
    and container IDs

    In container_1567013493699_0003_01_000002, attempt num is 01

    In attempt_201601081945_0005_m_000005_0, attempt num is 0
    """
    return (_id_part(d.get('attempt_id'), 5) or
            _id_part(d.get('container_id'), 3))


def _add_implied_task_id(d):
    """If *d* (a dictionary) has *attempt_id* but not *task_id*, add it.

    Use this on errors.
    """
    # NOTE: container IDs look similar to task IDs, but there's a single
    # numbering system for both map and reduce tasks
    if d.get('attempt_id') and not d.get('task_id'):
        d['task_id'] = _attempt_id_to_task_id(
            d['attempt_id'])


def _add_implied_job_id(d):
    """If *d* has *task_id* or *application_id* but not *job_id*,
    add it.

    (We don't infer application_id from job_id because application_id
    only exists on YARN)

    .. note::

       Don't use this with Spark, where job and application IDs don't
       necessarily match.
    """
    if not d.get('job_id'):
        if d.get('task_id'):
            d['job_id'] = _to_job_id(d['task_id'])
        elif d.get('application_id'):
            d['job_id'] = _to_job_id(d['application_id'])


def _attempt_id_to_task_id(attempt_id):
    """Convert e.g. ``'attempt_201601081945_0005_m_000005_0'``
    to ``'task_201601081945_0005_m_000005'``"""
    return 'task_' + '_'.join(attempt_id.split('_')[1:5])


def _to_job_id(task_id):
    """Convert e.g. ``'task_201601081945_0005_m_000005'``
    or ``'application_201601081945_0005'`` to
    to ``'job_201601081945_0005'``."""
    return 'job_' + '_'.join(task_id.split('_')[1:3])
