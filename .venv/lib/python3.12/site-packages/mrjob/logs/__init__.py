# -*- coding: utf-8 -*-
# Copyright 2015-2016 Yelp and Contributors
# Copyright 2017 Yelp
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
"""Utilities for parsing and interpreting logs.

There is one module for each kind of logs:

history: high-level job history info (found in <log dir>/history/)
step: stderr of `hadoop jar` command (so named because on EMR it appears in
    <log dir>/steps/)
task: stderr and syslog of individual tasks (found in <log dir>/userlogs/)

Other fields at the top level:

step_id: step ID (e.g. s-XXXXXXXX on EMR)
no_job: don't expect to find job/application ID (e.g. EMR's script-runner.jar)


Each of these should have methods like this:



_ls_*_logs(fs, log_dir_stream, **filter_kwargs):

     Find paths of all logs of this type.

     log_dir_stream is a list of lists of log dirs. We assume that you might
     have multiple ways to fetch the same logs (e.g. from S3, or by SSHing to
     nodes), so once we find a list of log dirs that works, we stop searching.

     This yields dictionaries with at least the key 'path' (path/URI
     of log) and possibly *_id fields as well (application_id, attempt_id,
     container_id, job_id, task_id)

     filter_kwargs allows us to filter by job ID, etc.

     Usually this is implemented with mrjob.logs.wrap_._ls_logs() and
     the _match_*_log_path() method (see below)


_match_*_log_path(path, **filter_kwargs):

    Is this the path of a log of this type?

    If there is a match, returns a dictionary. If not, returns None

    The match dictionary may be empty, but it can also include
    *_ids fields parsed from the path (*application_id*, *attempt_id*,
    *container_id*, *job_id*, *task_id*), or information about which
    version of Hadoop this file comes from (*yarn*).

    filter_kwargs allows us to filter by job ID, etc.


_interpret_*_log(fs, matches):
_interpret_*_logs(fs, matches, partial=True):

    Search one or more logs (or command stderr) for relevant information
    (counters, errors, and IDs).

    Rather than taking paths, takes a stream of matches returned by
    _ls_*_logs() (see above).

    If partial is set to True, just scan for the first error, starting
    with the last log.

    This returns a dictionary with the following format (all fields optional):

    application_id: YARN application ID for the step
    counters: group -> counter -> amount
    errors: [
        hadoop_error:  (for errors internal to Hadoop)
            message: string representation of Java stack trace
            path: URI of log file containing error
            start_line: first line of <path> with error (0-indexed)
            num_lines: # of lines containing error
        split:  (input split processed when error happened)
            path: URI of input
            start_line: first line read by this attempt
            num_lines: # of lines read by this attempt
        task_error:   (for errors caused by one task)
            message: string representation of error (e.g. Python command line
                followed by Python exception)
            path: (see above)
            start_line: (see above)
            num_lines: (see above)
        attempt_id: task attempt that this error originated from
        container_id: YARN container where error originated from
        task_id: task that this error originated from
    ]
    job_id: job ID for the step
    partial: set to true if we stopped parsing after the first error

    Errors' task_id should always be set if attempt_id is set (use
    mrjob.logs.id._add_implied_task_id()) and job_id should always be set
    if application_id is set (use mrjob.logs.id._add_implied_job_id)


_interpret_hadoop_jar_command_stderr(stderr, record_callback=None):

    Reads hadoop jar command output on the fly, but otherwise works like
    other _interpret_*() functions (same return format).


_parse_*_log(lines):

    Pull important information from a log file. This generally follows the same
    format as _interpret_<type>_logs(), above.

    Log lines are always strings (see mrjob.logs.wrap._cat_log_lines()).

    _parse_*_log() methods generally return a part of the _interpret_*_logs()
    format, but are *not* responsible for including implied job/task IDs.


_parse_*_records(lines):

    Helper method which parses low-level records out of a given log type.


There is one module for each kind of entity we want to deal with:

    counters: manipulating and printing counters
    errors: picking the best error, error reporting
    ids: handles parsing IDs and sorting IDs by recency

Finally:

    log4j: handles log4j record parsing (used by step and task syslog)
    wrap: module for listing and catting logs in an error-free
        way (since log parsing shouldn't kill a job).
"""
