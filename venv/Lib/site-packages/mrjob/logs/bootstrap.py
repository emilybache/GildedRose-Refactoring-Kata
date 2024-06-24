# Copyright 2015-2016 Yelp
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
"""Parse logs from EMR bootstrap actions (and, eventually, Dataproc
initialization actions)."""
import re

from .task import _parse_task_stderr
from .wrap import _cat_log_lines
from .wrap import _ls_logs

# match cause of failure when there's a problem with bootstrap script. Example:
#
# On the master instance (i-96c21a39), bootstrap action 1 returned a non-zero
# return code
#
# On 2 slave instances (including i-105af6bf and i-b659f519), bootstrap action
# 1 returned a non-zero return code
#
# (EMR doesn't seem to return errors that include both master and slave
# instances)
_BOOTSTRAP_NONZERO_RETURN_CODE_RE = re.compile(
    r'^.*\(.*?(?P<node_id>i-[0-9a-f]+).*\)'
    r'.*bootstrap action (?P<action_num>\d+)'
    r'.*non-zero return code'
    r'.*$')

# match a path like:
# <s3_log_dir>/node/i-96c21a39/bootstrap-actions/1/stderr.gz
#
# (may or may not actually be gzipped)
_EMR_BOOTSTRAP_STDERR_PATH_RE = re.compile(
    r'^(?P<prefix>.*?/)'
    r'node/'
    r'(?P<node_id>i-[0-9a-f]+)/'
    r'bootstrap-actions/'
    r'(?P<action_num>\d+)/'
    r'stderr(?P<suffix>\.\w+)?')


def _check_for_nonzero_return_code(reason):
    """Given a reason for cluster termination, check if it's because
    a bootstrap action terminated with an error.

    If it is, return a dictionary with the keys action_num (0-indexed
    bootstrap action number) and node_id (a string). Otherwise return None.
    """
    m = _BOOTSTRAP_NONZERO_RETURN_CODE_RE.match(reason)

    if m:
        return _extract_action_num_and_node_id(m)
    else:
        return None


def _ls_emr_bootstrap_stderr_logs(
        fs, log_dir_stream, action_num=None, node_id=None):
    """Find all stderr from bootstrap actions in the given dir. Sort
    so the most recent one comes first, using node ID as a tiebreaker.

    (In practice, we look at a single a action on a single node anyway.)
    """
    matches = _ls_logs(fs, log_dir_stream, _match_emr_bootstrap_stderr_path,
                       action_num=None, node_id=None)

    return sorted(matches, key=lambda m: (-m['action_num'], m['node_id']))


def _match_emr_bootstrap_stderr_path(path, node_id=None, action_num=None):
    """If *path* corresponds to a bootstrap stderr file, return a dict
    with the keys *action_num* (an 0-indexed int) and *node_id*. Otherwise
    return None.

    Optionally, filter by *action_num* and *node_id*.
    """
    m = _EMR_BOOTSTRAP_STDERR_PATH_RE.match(path)
    if not m:
        return

    result = _extract_action_num_and_node_id(m)

    if action_num is not None and action_num != result['action_num']:
        return None

    if node_id is not None and node_id != result['node_id']:
        return None

    return result


# This strategy assumes we can ask the EMR API which node(s) the error
# occurred on. This is true even after the cluster has shut down, so it's
# a pretty reasonable assumption, even for after-the-fact log parsing.
#
# If we *had* to figure out from logs alone whether a node had an error,
# we'd want to first check the controller file for a line like:
#
# 2016-07-07T23:26:49.565Z ERROR Execution failed with code '1'
#
# and then look in the corresponding stderr file, much like how we handle
# task logs. This seems like overkill at the moment.
def _interpret_emr_bootstrap_stderr(fs, matches, partial=True):
    """Extract errors from bootstrap stderr.

    If *partial* is true, stop when we find the first match.

    (In practice, we usually target a single file anyway.)
    """
    result = {}

    for match in matches:
        stderr_path = match['path']

        task_error = _parse_task_stderr(_cat_log_lines(fs, stderr_path))
        if task_error:
            task_error = dict(task_error)  # make a copy
            task_error['path'] = stderr_path
            error = dict(
                action_num=match['action_num'],
                node_id=match['node_id'],
                task_error=task_error)
            result.setdefault('errors', [])
            result['errors'].append(error)

            if partial:
                result['partial'] = True
                break

    return result


def _extract_action_num_and_node_id(m):
    """Helper method: Extract *action_num* and *node_id* from the given regex
    match. Convert *action_num* to a 0-indexed integer."""
    return dict(
        action_num=(int(m.group('action_num')) - 1),
        node_id=m.group('node_id'),
    )
