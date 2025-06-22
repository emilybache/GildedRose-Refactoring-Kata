# -*- coding: utf-8 -*-
# Copyright 2015-2016 Yelp
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
"""Parse the log4j syslog format used by Hadoop."""
import re
from logging import getLogger

from mrjob.py2 import to_unicode

# log line format output by hadoop jar command
_HADOOP_LOG4J_LINE_RE = re.compile(
    r'^\s*(?P<timestamp>.*?)'
    r'\s+(?P<level>[A-Z]+)'
    r'\s+(?P<logger>\S+)'
    r'(\s+\((?P<thread>.*?)\))?'
    r'( - |: )'
    r'(?P<message>.*?)$')

# log line format output to Hadoop syslog
_HADOOP_LOG4J_LINE_ALTERNATE_RE = re.compile(
    r'^\s*(?P<timestamp>.*?)'
    r'\s+(?P<level>[A-Z]+)'
    r'(\s+\[(?P<thread>.*?)\])'
    r'\s+(?P<logger>\S+)'
    r'(\s+\((?P<caller_location>\S+)\))?'
    r'( - |: )'
    r'(?P<message>.*?)$')

log = getLogger(__name__)


def _parse_hadoop_log4j_records(lines, pre_filter=None):
    """Parse lines from a hadoop log into log4j records.

    Yield dictionaries with the following keys:
    caller_location -- e.g. 'YarnClientImpl.java:submitApplication(251)'
    level -- e.g. 'INFO'
    logger -- e.g. 'amazon.emr.metrics.MetricsSaver'
    message -- the actual message. If this is a multi-line message (e.g.
        for counters), the lines will be joined by '\n'
    num_lines -- how many lines made up the message
    start_line -- which line the message started on (0-indexed)
    thread -- e.g. 'main'. Defaults to ''
    timestamp -- unparsed timestamp, e.g. '15/12/07 20:49:28',
        '2015-08-22 00:46:18,411'

    Lines will be converted to unicode, and trailing \r and \n will be stripped
    from lines.

    If set, *pre_filter* will be applied to stripped lines. If it
    returns true, we'll return a fake record with message set to the line,
    num_lines and start_line set as normal, and everything else set to ''.

    Also yields fake records for leading non-log4j lines (trailing non-log4j
    lines are assumed to be part of a multiline message if not pre-filtered).
    """
    last_record = None

    for line_num, line in enumerate(lines):
        # convert from bytes to unicode, if needed, and strip trailing newlines
        line = to_unicode(line).rstrip('\r\n')

        def fake_record():
            return dict(
                caller_location='',
                level='',
                logger='',
                message=line,
                num_lines=1,
                start_line=line_num,
                thread='',
                timestamp='')

        # had to patch this in here to get _parse_hadoop_jar_command_stderr()'s
        # record_callback to fire on the correct line. The problem is that
        # we don't emit records until we see the next line (to handle
        # multiline records), so the callback would fire in the wrong order
        if pre_filter:
            if pre_filter(line):
                if last_record:
                    last_record['num_lines'] = (
                        line_num - last_record['start_line'])
                    yield last_record

                yield fake_record()

                last_record = None
                continue

        m = (_HADOOP_LOG4J_LINE_RE.match(line) or
             _HADOOP_LOG4J_LINE_ALTERNATE_RE.match(line))

        if m:
            if last_record:
                last_record['num_lines'] = (
                    line_num - last_record['start_line'])
                yield last_record

            last_record = m.groupdict()
            last_record.setdefault('caller_location', '')
            last_record['thread'] = last_record['thread'] or ''
            last_record['start_line'] = line_num
        else:
            # add on to previous record
            if last_record:
                last_record['message'] += '\n' + line
            else:
                yield fake_record()

    if last_record:
        last_record['num_lines'] = (
            line_num + 1 - last_record['start_line'])
        yield last_record
