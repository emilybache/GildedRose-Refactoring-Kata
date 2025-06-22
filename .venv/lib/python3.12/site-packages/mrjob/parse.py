# Copyright 2009-2012 Yelp
# Copyright 2013 Steve Johnson and David Marin
# Copyright 2014 Yelp and Contributors
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
"""Utilities for parsing errors, counters, and status messages."""
import logging
import re
from functools import wraps
from io import BytesIO
from os.path import abspath

from mrjob.py2 import ParseResult
from mrjob.py2 import pathname2url
from mrjob.py2 import to_unicode
from mrjob.py2 import urljoin
from mrjob.py2 import urlparse as urlparse_buggy

log = logging.getLogger(__name__)


### URI PARSING ###

def is_uri(uri):
    r"""Return True if *uri* is a URI and contains ``://``
    (we only care about URIs that can describe files)
    """
    return '://' in uri and bool(urlparse(uri).scheme)


def is_s3_uri(uri):
    """Return True if *uri* can be parsed into an S3 URI, False otherwise."""
    try:
        parse_s3_uri(uri)
        return True
    except ValueError:
        return False


def parse_s3_uri(uri):
    """Parse an S3 URI into (bucket, key)

    >>> parse_s3_uri('s3://walrus/tmp/')
    ('walrus', 'tmp/')

    If ``uri`` is not an S3 URI, raise a ValueError
    """
    components = urlparse(uri)
    if (components.scheme not in ('s3', 's3n', 's3a') or
        '/' not in components.path):  # noqa

        raise ValueError('Invalid S3 URI: %s' % uri)

    return components.netloc, components.path[1:]


def to_uri(path_or_uri):
    """If *path_or_uri* is not a URI already, convert it to a ``file:///``
    URI."""
    if is_uri(path_or_uri):
        return path_or_uri
    else:
        return urljoin('file:', pathname2url(abspath(path_or_uri)))


@wraps(urlparse_buggy)
def urlparse(urlstring, scheme='', allow_fragments=True, *args, **kwargs):
    """A wrapper for :py:func:`urlparse.urlparse` that splits the fragment
    correctly in all URIs, not just Web-related ones.
    This behavior was fixed in the Python 2.7.4 standard library but we have
    to back-port it for previous versions.
    """
    (scheme, netloc, path, params, query, fragment) = (
        urlparse_buggy(urlstring, scheme, allow_fragments, *args, **kwargs))

    if allow_fragments and '#' in path and not fragment:
        path, fragment = path.split('#', 1)

    return ParseResult(scheme, netloc, path, params, query, fragment)


### OPTION PARSING ###


# planning to move this into mrjob.options
def _parse_port_range_list(range_list_str):
    all_ranges = []
    for range_str in range_list_str.split(','):
        if ':' in range_str:
            a, b = [int(x) for x in range_str.split(':')]
            all_ranges.extend(range(a, b + 1))
        else:
            all_ranges.append(int(range_str))
    return all_ranges


### parsing job output/stderr ###

_COUNTER_RE = re.compile(br'^reporter:counter:([^,]*),([^,]*),(-?\d+)$')
_STATUS_RE = re.compile(br'^reporter:status:(.*)$')


def parse_mr_job_stderr(stderr, counters=None):
    """Parse counters and status messages out of MRJob output.

    :param stderr: a filehandle, a list of lines (bytes), or bytes
    :param counters: Counters so far, to update; a map from group (string to
                     counter name (string) to count.

    Returns a dictionary with the keys *counters*, *statuses*, *other*:

    - *counters*: counters so far; same format as above
    - *statuses*: a list of status messages encountered
    - *other*: lines (strings) that aren't either counters or status messages
    """
    # For the corresponding code in Hadoop Streaming, see ``incrCounter()`` in
    # http://svn.apache.org/viewvc/hadoop/mapreduce/trunk/src/contrib/streaming/src/java/org/apache/hadoop/streaming/PipeMapRed.java?view=markup  # noqa
    if isinstance(stderr, bytes):
        stderr = BytesIO(stderr)

    if counters is None:
        counters = {}
    statuses = []
    other = []

    for line in stderr:
        m = _COUNTER_RE.match(line.rstrip(b'\r\n'))
        if m:
            group, counter, amount_str = m.groups()

            # don't leave these as bytes on Python 3
            group = to_unicode(group)
            counter = to_unicode(counter)

            counters.setdefault(group, {})
            counters[group].setdefault(counter, 0)
            counters[group][counter] += int(amount_str)
            continue

        m = _STATUS_RE.match(line.rstrip(b'\r\n'))
        if m:
            # don't leave as bytes on Python 3
            statuses.append(to_unicode(m.group(1)))
            continue

        other.append(to_unicode(line))

    return {'counters': counters, 'statuses': statuses, 'other': other}


### job tracker/resource manager ###

_JOB_TRACKER_HTML_RE = re.compile(br'\b(\d{1,3}\.\d{2})%')
_RESOURCE_MANAGER_JS_RE = re.compile(
    br'\s*\[.*application_[_\d]+.*"RUNNING"'
    br'.*width:(?P<percent>\d{1,3}.\d)%.*\]'
)


def _parse_progress_from_job_tracker(html_bytes):
    """Pull (map_percent, reduce_percent) from running job from job tracker
    HTML as floats, or return (None, None).

    This assumes at most one running job (designed for EMR).
    """
    # snip out the Running Jobs section (ignore the header)
    start = html_bytes.rfind(b'Running Jobs')
    if start == -1:
        return None, None
    end = html_bytes.find(b'Jobs', start + len(b'Running Jobs'))
    if end == -1:
        end = None

    html_bytes = html_bytes[start:end]

    # search it for percents
    matches = _JOB_TRACKER_HTML_RE.findall(html_bytes)
    if len(matches) >= 2:
        return float(matches[0]), float(matches[1])
    else:
        return None, None


def _parse_progress_from_resource_manager(html_bytes):
    """Pull progress_precent for running job from job tracker HTML, as a
    float, or return None.

    This assumes at most one running job (designed for EMR).
    """
    # this is for EMR and assumes only one running job
    for line in html_bytes.splitlines():
        m = _RESOURCE_MANAGER_JS_RE.match(line)
        if m:
            return float(m.group('percent'))

    return None
