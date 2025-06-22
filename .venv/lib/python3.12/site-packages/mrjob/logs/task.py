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
"""Parse "task" logs, which are the syslog and stderr for each individual
task and typically appear in the userlogs/ directory."""
from collections import defaultdict
from logging import getLogger
import re

from .ids import _add_implied_task_id
from .ids import _to_job_id
from .log4j import _parse_hadoop_log4j_records
from .wrap import _cat_log_lines
from .wrap import _ls_logs
from mrjob import parse

log = getLogger(__name__)

# Match a java exception, possibly preceded by 'PipeMapRed failed!', etc.
# use this with search()
_JAVA_TRACEBACK_RE = re.compile(
    r'\s+at .*\((.*\.(java|scala):\d+|Native Method)\)$',
    re.MULTILINE)

# Match an error stating that Spark's subprocess has failed (and thus we
# should read stdout
_SPARK_APP_EXITED_RE = re.compile(
    r'^\s*User application exited with status \d+\s*$')

# the name of the logger that logs the above
_SPARK_APP_MASTER_LOGGER = 'ApplicationMaster'

# this seems to only happen for S3. Not sure if this happens in YARN
_OPENING_FOR_READING_RE = re.compile(
    r"^Opening '(?P<path>.*?)' for reading$")

# what log paths look like pre-YARN
_PRE_YARN_TASK_LOG_PATH_RE = re.compile(
    r'^(?P<prefix>.*?/)'
    r'(?P<attempt_id>attempt_(?P<timestamp>\d+)_(?P<step_num>\d+)_'
    r'(?P<task_type>[mr])_(?P<task_num>\d+)_'
    r'(?P<attempt_num>\d+))/'
    r'(?P<log_type>stderr|syslog)(?P<suffix>\.\w{1,3})?$')

# ignore counters and status (only happens in sim runners, where task stderr
# is dumped straight to a file).

# convert from bytes regex to text regex
_COUNTER_RE = re.compile(parse._COUNTER_RE.pattern.decode('ascii'))
_STATUS_RE = re.compile(parse._STATUS_RE.pattern.decode('ascii'))

# ignore warnings about initializing log4j in task stderr
_LOG4J_WARN_RE = re.compile(r'^log4j:WARN .*$')

# also ignore counters and status messages (this only happens in
# local mode, where there's no real Hadoop to filter them out)
_TASK_STDERR_IGNORE_RES = [
    _COUNTER_RE,
    _STATUS_RE,
    _LOG4J_WARN_RE,
]

# this is the start of a Java stacktrace that Hadoop 1 always logs to
# stderr when tasks fail (see #1430)
_SUBPROCESS_FAILED_STACK_TRACE_START = re.compile(
    r'^java\.lang\.RuntimeException: PipeMapRed\.waitOutputThreads\(\):'
    r' subprocess failed with code .*$')

# message telling us about a (input) split. Looks like this:
#
# Processing split: hdfs://ddf64167693a:9000/path/to/bootstrap.sh:0+335
_YARN_INPUT_SPLIT_RE = re.compile(
    r'^Processing split:\s+(?P<path>.*)'
    r':(?P<start_line>\d+)\+(?P<num_lines>\d+)$')

# what log paths look like on YARN (also used for Spark, hence stdout)
_YARN_TASK_LOG_PATH_RE = re.compile(
    r'^(?P<prefix>.*?/)'
    r'(?P<application_id>application_\d+_\d{4})/'
    r'(?P<container_id>container(_\d+)+)/'
    r'(?P<log_type>stderr|stdout|syslog)(?P<suffix>\.\w{1,3})?$')


def _ls_task_logs(fs, log_dir_stream, application_id=None, job_id=None,
                  error_attempt_ids=None, attempt_to_container_id=None):
    """Yield matching logs, optionally filtering by application_id
    or job_id.

    This will yield matches for stderr logs first, followed by syslogs. stderr
    logs will have a 'syslog' field pointing to the match for the
    corresponding syslog (stderr logs without a corresponding syslog won't be
    included).
    """
    return _ls_task_logs_helper(
        fs, log_dir_stream, is_spark=False,
        application_id=application_id, job_id=job_id,
        error_attempt_ids=error_attempt_ids,
        attempt_to_container_id=attempt_to_container_id)


def _ls_spark_task_logs(
        fs, log_dir_stream, application_id=None, job_id=None,
        error_attempt_ids=None, attempt_to_container_id=None):
    """Yield matching Spark logs, optionally filtering by application_id
    or job_id.

    This will yield matches for stderr logs only. stderr
    logs will have a 'stdout' field pointing to the match for the
    corresponding stdout file; whether we process this depends on the content
    of the stderr file.
    """
    return _ls_task_logs_helper(
        fs, log_dir_stream, is_spark=True,
        application_id=application_id, job_id=job_id,
        error_attempt_ids=error_attempt_ids,
        attempt_to_container_id=attempt_to_container_id)


def _ls_task_logs_helper(fs, log_dir_stream, is_spark,
                         application_id=None, job_id=None,
                         error_attempt_ids=None, attempt_to_container_id=None):
    """Helper for _ls_task_logs() and _ls_spark_task_logs().

    *syslog_type* is the type of the log to pair with stderr logs.

    This is actually a bit weird; on Spark, 'stderr' is the equivalent
    of syslog in Streaming, and 'stdout' is the equivlend of Streaming's
    stderr.

    For Streaming, we want stderr logs with corresponding syslogs, and if after
    listing all task logs we don't find any, syslogs.

    For Spark, we want stderr logs (which are equivalent to Streaming syslogs)
    with corresponding stdouts, and if after listing all task logs we don't
    find any, stderr logs without corresponding stdouts.
    """
    syslog_type = 'stdout' if is_spark else 'syslog'

    error_attempt_ids = error_attempt_ids or ()

    # figure out subdirs to look for logs in
    if attempt_to_container_id:
        # YARN
        subdirs = [
            attempt_to_container_id[a] for a in error_attempt_ids
            if a in attempt_to_container_id]
    else:
        subdirs = list(error_attempt_ids)

    # only look in subdirs corresponding to failed attempts
    if subdirs:
        log_subdir_stream = ([
            fs.join(log_dir, subdir)
            for subdir in subdirs
            for log_dir in log_dir_list
        ] for log_dir_list in log_dir_stream)
    else:
        log_subdir_stream = log_dir_stream

    key_to_type_to_match = defaultdict(dict)

    # less desirable errors to yield if we don't find the ones we want
    other_matches = []

    for match in _ls_logs(
            fs, log_subdir_stream, _match_task_log_path, is_spark,
            application_id=application_id,
            job_id=job_id):

        log_key = _log_key(match)
        log_type = match['log_type']

        if log_type not in ('stderr', syslog_type):
            continue  # don't care

        type_to_match = key_to_type_to_match[log_key]

        if log_type in type_to_match:
            continue  # already seen

        type_to_match[log_type] = match

        # yield stderrs with syslogs as we find them
        if 'stderr' in type_to_match and syslog_type in type_to_match:
            stderr_match = type_to_match['stderr']
            syslog_match = type_to_match[syslog_type]

            stderr_match[syslog_type] = syslog_match

            yield stderr_match

        if log_type == ('stderr' if is_spark else syslog_type):
            other_matches.append(match)

    # yield logs that don't have both syslog and stderr
    for other_match in other_matches:
        if syslog_type not in other_match:  # already yielded
            yield other_match


def _log_key(match):
    """Helper method for _ls_task_logs() and _ls_spark_task_logs()."""
    return tuple((k, v) for k, v in sorted(match.items())
                 if k not in ('log_type', 'path'))


def _match_task_log_path(path, application_id=None, job_id=None):
    """Is this the path/URI of a task log? (Including Spark)

    If so, return a dictionary containing application_id and container_id
    (on YARN) or attempt_id (on pre-YARN Hadoop), plus log_type (either
    stdout, stderr, or syslog).

    Otherwise, return None

    Optionally, filter by application_id (YARN) or job_id (pre-YARN).
    """
    m = _PRE_YARN_TASK_LOG_PATH_RE.match(path)
    if m:
        if job_id and job_id != _to_job_id(m.group('attempt_id')):
            return None  # matches, but wrong job_id

        return dict(
            attempt_id=m.group('attempt_id'),
            log_type=m.group('log_type'))

    m = _YARN_TASK_LOG_PATH_RE.match(path)
    if m:
        if application_id and application_id != m.group('application_id'):
            return None  # matches, but wrong application_id

        return dict(
            application_id=m.group('application_id'),
            container_id=m.group('container_id'),
            log_type=m.group('log_type'))

    return None


def _interpret_task_logs(fs, matches, partial=True, log_callback=None):
    """Look for errors in task syslog/stderr.

    If *partial* is true (the default), stop when we find the first error
    that includes a *task_error*.

    If *log_callback* is set, every time we're about to parse a
        file, call it with a single argument, the path of that file

    Returns a dictionary possibly containing the key 'errors', which
    is a dict containing:

    hadoop_error:
        message: string containing error message and Java exception
        num_lines: number of lines in syslog this takes up
        path: syslog we read this error from
        start_line: where in syslog exception starts (0-indexed)
    split: (optional)
        path: URI of input file task was processing
        num_lines: (optional) number of lines in split
        start_line: (optional) first line of split (0-indexed)
    task_error:
        message: command and error message from task, as a string
        num_lines: number of lines in stderr this takes up
        path: stderr we read this from
        start_line: where in stderr error message starts (0-indexed)

    In addition, if *partial* is set to true (and we found an error),
    this dictionary will contain the key *partial*, set to True.
    """
    result = {}
    syslogs_parsed = set()

    for match in matches:
        error = {}

        # are is this match for a stderr file, or a syslog?
        if match.get('syslog'):
            stderr_path = match['path']
            syslog_path = match['syslog']['path']
        else:
            stderr_path = None
            syslog_path = match['path']

        if stderr_path:
            if log_callback:
                log_callback(stderr_path)
            task_error = _parse_task_stderr(_cat_log_lines(fs, stderr_path))

            if task_error:
                task_error['path'] = stderr_path
                error['task_error'] = task_error
            else:
                continue  # can parse syslog independently later

        # already parsed this syslog in conjunction with an earlier task error
        if syslog_path in syslogs_parsed:
            continue

        if log_callback:
            log_callback(syslog_path)
        syslog_error = _parse_task_syslog(_cat_log_lines(fs, syslog_path))
        syslogs_parsed.add(syslog_path)

        if not syslog_error.get('hadoop_error'):
            # if no entry in Hadoop syslog, probably just noise
            continue

        error.update(syslog_error)
        error['hadoop_error']['path'] = syslog_path

        # patch in IDs we learned from path
        for id_key in 'attempt_id', 'container_id':
            if id_key in match:
                error[id_key] = match[id_key]
        _add_implied_task_id(error)

        result.setdefault('errors', [])
        result['errors'].append(error)

        if partial:
            result['partial'] = True
            break

    return result


def _parse_task_syslog(lines):
    """Parse an error out of a syslog file (or a Spark stderr file).

    Returns a dict, possibly containing the following keys:

    check_stdout:
        if true, we should look for task errors in the corresponding
        'stdout' file. Used for Spark logs.
    hadoop_error:
        message: string containing error message and Java exception
        num_lines: number of lines in syslog this takes up
        start_line: where in syslog exception starts (0-indexed)
    split: (optional)
        path: URI of input file task was processing
        num_lines: (optional) number of lines in split
        start_line: (optional) first line of split (0-indexed)
    """
    return _parse_task_syslog_records(_parse_hadoop_log4j_records(lines))


def _parse_task_syslog_records(records):
    """Helper for _parse_task_syslog(); takes log4j records rather than
    lines"""
    result = {}

    for record in records:
        message = record['message']

        m = _OPENING_FOR_READING_RE.match(message)
        if m:
            result['split'] = dict(path=m.group('path'))
            continue

        m = _YARN_INPUT_SPLIT_RE.match(message)
        if m:
            result['split'] = dict(
                path=m.group('path'),
                start_line=int(m.group('start_line')),
                num_lines=int(m.group('num_lines')))
            continue

        m = _JAVA_TRACEBACK_RE.search(message)
        if m:
            result['hadoop_error'] = dict(
                message=message,
                num_lines=record['num_lines'],
                start_line=record['start_line'],
            )
            break  # nothing to do once we've found the error

        if (record['logger'] == _SPARK_APP_MASTER_LOGGER and
                record['level'] == 'ERROR'):
            m = _SPARK_APP_EXITED_RE.match(message)
            if m:
                result['hadoop_error'] = dict(
                    message=message,
                    num_lines=record['num_lines'],
                    start_line=record['start_line'],
                )
                result['check_stdout'] = True
                break  # nothing else to do once we've found the error

    return result


# TODO: allow filtering of bad lines to happen elsewhere, pass this
# function numbered lines
def _parse_task_stderr(lines):
    """Attempt to explain any error in task stderr, be it a Python
    exception or a problem with a setup command (see #1203).

    Looks for '+ ' followed by a command line, and then the command's
    stderr. If there are no such lines (because we're not using a setup
    script), assumes the entire file contents are the cause of error.

    Returns a task error dictionary with the following keys, or None
    if the file is empty.

    message: a string (e.g. Python command line followed by Python traceback)
    start_line: where in lines message appears (0-indexed)
    num_lines: how may lines the message takes up
    """
    task_error = None
    stack_trace_start_line = None

    for line_num, line in enumerate(lines):
        line = line.rstrip('\r\n')

        # ignore "subprocess failed" stack trace
        if _SUBPROCESS_FAILED_STACK_TRACE_START.match(line):
            stack_trace_start_line = line_num
            continue

        # once we detect a stack trace, keep ignoring lines until
        # we find a non-indented one
        if stack_trace_start_line is not None:
            if line.lstrip() != line:
                continue
            else:
                stack_trace_start_line = None

        # ignore warnings about initializing log4j, counters, etc.
        if any(ir.match(line) for ir in _TASK_STDERR_IGNORE_RES):
            # ignored lines shouldn't count as part of the line range
            if task_error and task_error.get('num_lines') is None:
                task_error['num_lines'] = line_num - task_error['start_line']
            continue
        elif not task_error or line.startswith('+ '):
            # stderr log should only contain counters and status
            # messages in local mode
            task_error = dict(
                message=line,
                start_line=line_num)
        else:
            task_error['message'] += '\n' + line
            task_error['num_lines'] = None

    if task_error:
        if task_error.get('num_lines') is None:
            end_line = stack_trace_start_line or (line_num + 1)
            task_error['num_lines'] = end_line - task_error['start_line']
        return task_error
    else:
        return None
