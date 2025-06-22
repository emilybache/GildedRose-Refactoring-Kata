# -*- coding: utf-8 -*-
# Copyright 2015-2017 Yelp
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
"""Parse "step" logs, which are the stdout/stderr/syslog of the
Hadoop streaming command (we call them this because EMR puts them
in the steps/ subdir of the logs on S3)."""
import errno
import re
import logging
from logging import getLogger

from .ids import _add_implied_job_id
from .ids import _add_implied_task_id
from .log4j import _parse_hadoop_log4j_records
from .task import _parse_task_stderr
from .wrap import _cat_log_lines
from .wrap import _ls_logs


# path of step logs (these only exist on EMR). Step logs on S3 are
# rotated and timestamped with YYYY-MM-DD-HH
_EMR_STEP_LOG_PATH_RE = re.compile(
    r'^(?P<prefix>.*?/)'
    r'(?P<step_id>s-[A-Z0-9]+)/'
    r'(?P<log_type>syslog|stderr)'
    r'(\.(?P<timestamp>[\d-]+))?(?P<suffix>\.\w+)?$')

# hadoop streaming always prints "packageJobJar..." to stdout,
# and prints Streaming Command Failed! to stderr on failure
_HADOOP_STREAMING_NON_LOG4J_LINE_RE = re.compile(
    r'^(packageJobJar: .*|Streaming Command Failed!)$')

# first line of counters message
_INDENTED_COUNTERS_MESSAGE_RE = re.compile(
    r'^Counters: (?P<num_counters>\d+)\s*$', re.MULTILINE)

# header for a group of counters
_INDENTED_COUNTER_GROUP_RE = re.compile(r'^(?P<indent>\s+)(?P<group>.*)$')

# line for a counter
_INDENTED_COUNTER_RE = re.compile(
    r'^(?P<indent>\s+)(?P<counter>.*)=(?P<amount>\d+)\s*$')

# line specifying output directory
_OUTPUT_DIRECTORY_RE = re.compile(
    r'^Output( directory)?:'
    r'\s+(?P<output_dir>\S+://\S+)\s*$')

# how to get application_id from YARN
_SUBMITTED_APPLICATION_RE = re.compile(
    r'^Submitted application (?P<application_id>application_\d+_\d{4})\s*$')

# how to get job_id (all versions)
_RUNNING_JOB_RE = re.compile(
    r'^Running job: (?P<job_id>job_\d+_\d{4})\s*$')

# job progress (YARN)
# no need to make this work for pre-YARN, only Dataproc runner uses it
_JOB_PROGRESS_RE = re.compile(
    r'^\s*map\s+(?P<map>\d+)%\s+reduce\s+(?P<reduce>\d+)%\s*$')

# if you specify a bad jar, this is all you get
_NOT_A_VALID_JAR_RE = re.compile(r'^\s*Not a valid JAR:.*')

# YARN prints this (sometimes followed by a Java exception) when tasks fail
_TASK_ATTEMPT_FAILED_RE = re.compile(
    r'^Task Id *:'
    r' (?P<attempt_id>attempt_\d+_\d{4}_[mr]_\d+_\d+),'
    r' Status *: FAILED$',
    re.MULTILINE)

log = getLogger(__name__)


def _ls_emr_step_syslogs(fs, log_dir_stream, step_id=None):
    """Yield matching step logs, optionally filtering by *step_id*.
    Yields dicts with the keys:

    path: path/URI of step file
    step_id: step_id in *path* (must match *step_id* if set)
    """
    matches = _ls_logs(fs, log_dir_stream, _match_emr_step_syslog_path,
                       step_id=step_id)

    return sorted(matches, key=_match_sort_key)


def _ls_emr_step_stderr_logs(fs, log_dir_stream, step_id=None):
    """Yield matching step logs, optionally filtering by *step_id*.
    Yields dicts with the keys:

    path: path/URI of step file
    step_id: step_id in *path* (must match *step_id* if set)
    """
    matches = _ls_logs(fs, log_dir_stream, _match_emr_step_stderr_path,
                       step_id=step_id)

    # we basically want to tail the stderr log, so search rotated
    # logs in reverse
    return sorted(matches, key=_match_sort_key, reverse=True)


def _match_sort_key(m):
    """sort key which treats empty timestamp as most recent"""
    return (m['timestamp'] is None, m['timestamp'] or '')


def _match_emr_step_syslog_path(path, step_id=None):
    """Match path of a step syslog, optionally filtering by *step_id*.

    If there is a match, return a dict with the keys *step_id* and
    *timestamp* (a string). Otherwise, returns None.
    """
    return _match_emr_step_log_path(path, 'syslog', step_id=step_id)


def _match_emr_step_stderr_path(path, step_id=None):
    """Match path of a step stderr log, optionally filtering by *step_id*.

    If there is a match, return a dict with the keys *step_id* and
    *timestamp* (a string). Otherwise, returns None.
    """
    return _match_emr_step_log_path(path, 'stderr', step_id=step_id)


def _match_emr_step_log_path(path, log_type, step_id=None):
    """Helper for :py:func:`_match_emr_step_syslog_path` and
    :py:func:`_match_emr_step_stderr_path`
    """
    m = _EMR_STEP_LOG_PATH_RE.match(path)
    if not m:
        return None

    if m.group('log_type') != log_type:
        return None

    if not (step_id is None or m.group('step_id') == step_id):
        return None

    return dict(step_id=m.group('step_id'), timestamp=m.group('timestamp'))


def _interpret_emr_step_syslog(fs, matches):
    """Extract information from step syslog (see :py:func:`_parse_step_log()`),
    which may be split into several chunks by timestamp"""
    # going to merge results for each log into final result
    errors = []
    result = {}

    for match in matches:
        path = match['path']

        interpretation = _parse_step_syslog(_cat_log_lines(fs, path))

        result.update(interpretation)
        for error in result.get('errors') or ():
            if 'hadoop_error' in error:
                error['hadoop_error']['path'] = path
            _add_implied_task_id(error)
            errors.append(error)

    _add_implied_job_id(result)
    if errors:
        result['errors'] = errors

    return result


def _interpret_new_dataproc_step_stderr(step_interpretation, new_lines):
    """Incrementally update *step_interpretation* (a dict) with information
    from new lines read from Hadoop job driver output on Dataproc."""
    return _parse_step_syslog_from_log4j_records(
        _parse_hadoop_log4j_records(new_lines),
        step_interpretation)


def _interpret_emr_step_stderr(fs, matches):
    """Extract information from step stderr (see
    :py:func:`~mrjob.logs.task._parse_task_stderr()`),
    which may be split into several chunks by timestamp"""
    for match in matches:
        path = match['path']

        error = _parse_task_stderr(_cat_log_lines(fs, path))

        if error:
            error['path'] = path
            # We're essentially just tailing the stderr log, so stop when we
            # find an error.
            return dict(errors=[dict(task_error=error)])

    return {}


def _eio_to_eof(pty_master):
    """Yield lines from a PTY, gracefully handling an ``IOError`` with
    ``errno == EIO`` as end-of-file."""
    try:
        for line in pty_master:
            yield line
    except IOError as e:
        # this is just the PTY's way of saying goodbye
        if e.errno == errno.EIO:
            return
        else:
            raise


def _interpret_hadoop_jar_command_stderr(lines, record_callback=None):
    """Parse *lines* from the ``hadoop jar`` command's stderr (lines can be
    either bytes or unicode). Works like :py:func:`_parse_step_syslog` (same
    return format) with a few extra features to handle the output of the
    ``hadoop jar`` command on the fly:

    - Pre-filters non-log4j stuff from Hadoop Streaming so it doesn't
      get treated as part of a multi-line message
    - Optionally calls *record_callback* for each log4j record (see
      :py:func:`~mrjob.logs.log4j._parse_hadoop_log4j_records`).
    """
    def pre_filter(line):
        return bool(_HADOOP_STREAMING_NON_LOG4J_LINE_RE.match(line))

    def yield_records():
        for record in _parse_hadoop_log4j_records(
                lines, pre_filter=pre_filter):
            if record_callback:
                record_callback(record)
            yield record

    result = _parse_step_syslog_from_log4j_records(yield_records())

    _add_implied_job_id(result)
    for error in result.get('errors') or ():
        _add_implied_task_id(error)

    return result


def _parse_step_syslog(lines):
    """Parse the syslog from the ``hadoop jar`` command.

    Returns a dictionary which potentially contains the following keys:

    application_id: a string like 'application_1449857544442_0002'. Only
        set on YARN
    counters: a map from counter group -> counter -> amount, or None if
        no counters found (only YARN prints counters)
    errors: a list of errors, with the following keys:
        hadoop_error:
            message: lines of error, as as string
            start_line: first line of log containing the error (0-indexed)
            num_lines: # of lines of log containing the error
        attempt_id: ID of task attempt with this error
    job_id: a string like 'job_201512112247_0003'. Should always be set
    output_dir: a URI like 'hdfs:///user/hadoop/tmp/my-output-dir'. Should
        always be set on success.
    """
    return _parse_step_syslog_from_log4j_records(
        _parse_hadoop_log4j_records(lines))


def _parse_step_syslog_from_log4j_records(records, step_interpretation=None):
    """Pulls errors, counters, IDs, etc. from log4j records
    emitted by Hadoop.

    This powers :py:func:`_parse_step_syslog` and
    :py:func:`_interpret_hadoop_jar_command_stderr`.
    """
    if step_interpretation is None:
        result = {}
    else:
        result = step_interpretation

    for record in records:
        message = record['message']

        # counters
        if _is_counter_log4j_record(record):
            result['counters'] = _parse_indented_counters(
                message.splitlines())
            continue

        # output_dir
        m = _OUTPUT_DIRECTORY_RE.match(message)
        if m:
            result['output_dir'] = m.group('output_dir')
            continue

        # application_id
        m = _SUBMITTED_APPLICATION_RE.match(message)
        if m:
            result['application_id'] = m.group('application_id')
            continue

        # job_id
        m = _RUNNING_JOB_RE.match(message)
        if m:
            result['job_id'] = m.group('job_id')
            continue

        # progress
        m = _JOB_PROGRESS_RE.match(message)
        if m:
            result['progress'] = dict(
                map=int(m.group('map')),
                reduce=int(m.group('reduce')),
                message=message,
            )

        # invalid jar
        m = _NOT_A_VALID_JAR_RE.match(message)
        if m:
            error = dict(
                hadoop_error=dict(
                    message=message,
                    num_lines=record['num_lines'],
                    start_line=record['start_line'],
                ),
            )
            result.setdefault('errors', [])
            result['errors'].append(error)

        # task failure
        m = _TASK_ATTEMPT_FAILED_RE.match(message)
        if m:
            error_str = '\n'.join(message.splitlines()[1:])
            if not error_str:  # if no exception, print something
                error_str = message

            error = dict(
                attempt_id=m.group('attempt_id'),
                hadoop_error=dict(
                    message=error_str,
                    num_lines=record['num_lines'],
                    start_line=record['start_line'],
                )
            )

            result.setdefault('errors', [])
            result['errors'].append(error)

    return result


def _is_counter_log4j_record(record):
    """Is this the record containing counters?

    (HadoopJobRunner doesn't want to log this because it's very long
    and it has its own way of displaying counters.)
    """
    return bool(_INDENTED_COUNTERS_MESSAGE_RE.match(record['message']))


def _parse_indented_counters(lines):
    """Parse counters in the indented format output/logged by the
    Hadoop binary.

    Takes input as lines (should not include log record stuff) and
    returns a map from counter group to counter to amount.
    """
    counters = {}  # map group -> counter -> amount
    group = None
    group_indent = None

    for line in lines:
        if not (group is None or group_indent is None):
            m = _INDENTED_COUNTER_RE.match(line)
            if m and len(m.group('indent')) > group_indent:

                counter = m.group('counter')
                amount = int(m.group('amount'))

                counters.setdefault(group, {})
                counters[group][counter] = amount

                continue

        m = _INDENTED_COUNTER_GROUP_RE.match(line)
        if m:
            group = m.group('group')
            group_indent = len(m.group('indent'))

        elif not _INDENTED_COUNTERS_MESSAGE_RE.match(line):
            log.warning('unexpected counter line: %s' % line)

    return counters


def _log_line_from_driver(line, level=None):
    """Log ``'  <line>'``. *line* should be a string.

    Optionally specify a logging level (default is logging.INFO).
    """
    log.log(level or logging.INFO, '  %s' % line)


def _log_log4j_record(record):
    """Log a log4j message at the appropriate logging level"""
    level = getattr(logging, record.get('level') or '', None)
    _log_line_from_driver(record['message'], level=level)
