# Copyright 2009-2010 Yelp
# Copyright 2015-2019 Yelp
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
"""Audit EMR usage over the past 2 weeks, sorted by cluster name and user.

Usage::

    mrjob audit-emr-usage > report

Options::

  -c CONF_PATHS, --conf-path CONF_PATHS
                        Path to alternate mrjob.conf file to read from
  --no-conf             Don't load mrjob.conf even if it's available
  --ec2-endpoint EC2_ENDPOINT
                        Force mrjob to connect to EC2 on this endpoint (e.g.
                        ec2.us-west-1.amazonaws.com). Default is to infer this
                        from region.
  --emr-endpoint EMR_ENDPOINT
                        Force mrjob to connect to EMR on this endpoint (e.g.
                        us-west-1.elasticmapreduce.amazonaws.com). Default is
                        to infer this from region.
  -h, --help            show this help message and exit
  --max-days-ago MAX_DAYS_AGO
                        Max number of days ago to look at jobs. By default, we
                        go back as far as EMR supports (currently about 2
                        months)
  -q, --quiet           Don't print anything to stderr
  --region REGION       GCE/AWS region to run Dataproc/EMR jobs in.
  --s3-endpoint S3_ENDPOINT
                        Force mrjob to connect to S3 on this endpoint (e.g. s3
                        -us-west-1.amazonaws.com). You usually shouldn't set
                        this; by default mrjob will choose the correct
                        endpoint for each S3 bucket based on its location.
  -v, --verbose         print more messages to stderr
"""
# This just approximates EMR billing rules. For the actual rules, see:
#
# http://aws.amazon.com/elasticmapreduce/faqs/
from __future__ import print_function

import math
import logging
import re
from argparse import ArgumentParser
from datetime import datetime
from datetime import timedelta
from time import sleep

from mrjob.aws import _boto3_now
from mrjob.aws import _boto3_paginate
from mrjob.emr import EMRJobRunner
from mrjob.job import MRJob
from mrjob.options import _add_basic_args
from mrjob.options import _add_runner_args
from mrjob.options import _alphabetize_actions
from mrjob.options import _filter_by_role
from mrjob.pool import _pool_name
from mrjob.util import strip_microseconds

# match an mrjob job key (used to uniquely identify the job)
_JOB_KEY_RE = re.compile(r'^(.*)\.(.*)\.(\d+)\.(\d+)\.(\d+)$')

# match an mrjob step name (these are used to name steps in EMR)
_STEP_NAME_RE = re.compile(
    r'^(.*)\.(.*)\.(\d+)\.(\d+)\.(\d+): Step (\d+) of (\d+)$')

# wait one second between successive calls to EMR API
_DELAY = 1

log = logging.getLogger(__name__)


def main(args=None):
    # parse command-line args
    arg_parser = _make_arg_parser()

    options = arg_parser.parse_args(args)

    MRJob.set_up_logging(quiet=options.quiet, verbose=options.verbose)

    now = _boto3_now()

    log.info('getting cluster history...')
    clusters = list(_yield_clusters(
        max_days_ago=options.max_days_ago, now=now, **_runner_kwargs(options)))

    log.info('compiling cluster stats...')
    stats = _clusters_to_stats(clusters, now=now)

    _print_report(stats, now=now)


def _make_arg_parser():
    usage = '%(prog)s audit-emr-usage [options]'
    description = 'Print a giant report on EMR usage.'

    arg_parser = ArgumentParser(usage=usage, description=description)

    arg_parser.add_argument(
        '--max-days-ago', dest='max_days_ago', type=float, default=None,
        help=('Max number of days ago to look at jobs. By default, we go back'
              ' as far as EMR supports (currently about 2 months)'))

    _add_basic_args(arg_parser)
    _add_runner_args(
        arg_parser,
        _filter_by_role(EMRJobRunner.OPT_NAMES, 'connect'))

    _alphabetize_actions(arg_parser)

    return arg_parser


def _runner_kwargs(options):
    kwargs = options.__dict__.copy()
    for unused_arg in ('quiet', 'verbose', 'max_days_ago'):
        del kwargs[unused_arg]

    return kwargs


def _clusters_to_stats(clusters, now=None):
    r"""Aggregate statistics for several clusters into a dictionary.

    :param clusters: a sequence of dicts with the keys ``cluster``, ``steps``.
    :param now: the current UTC time, as a :py:class:`datetime.datetime`.
                Defaults to the current time.

    Returns a dictionary with many keys, including:

    * *summaries*: A list of dictionaries; the result of running
      :py:func:`_cluster_to_full_summary` on each cluster.

    total usage:

    * *nih_billed*: total normalized instances hours billed, for all clusters
    * *nih_used*: total normalized instance hours actually used for
      bootstrapping and running jobs.
    * *nih_bbnu*: total usage billed but not used (`nih_billed - nih_used`)

    further breakdown of total usage:

    * *bootstrap_nih_used*: total usage for bootstrapping
    * *end_nih_bbnu*: unused time at the end of clusters
    * *job_nih_used*: total usage for jobs (`nih_used - bootstrap_nih_used`)
    * *other_nih_bbnu*: other unused time (`nih_bbnu - end_nih_bbnu`)

    grouping by various keys:

    (There is a *_used*, *_billed*, and *_bbnu* version of all stats below)

    * *date_to_nih_\**: map from a :py:class:`datetime.date` to number
      of normalized instance hours on that date
    * *hour_to_nih_\**: map from a :py:class:`datetime.datetime` to number
      of normalized instance hours during the hour starting at that time
    * *label_to_nih_\**: map from jobs' labels (usually the module name of
      the job) to normalized instance hours, with ``None`` for
      non-:py:mod:`mrjob` jobs. This includes usage data for bootstrapping.
    * *job_step_to_nih_\**: map from jobs' labels and step number to
      normalized instance hours, using ``(None, None)`` for non-:py:mod:`mrjob`
      jobs. This does not include bootstrapping.
    * *job_step_to_nih_\*_no_pool*: Same as *job_step_to_nih_\**, but only
      including non-pooled clusters.
    * *owner_to_nih_\**: map from jobs' owners (usually the user who ran them)
      to normalized instance hours, with ``None`` for non-:py:mod:`mrjob` jobs.
      This includes usage data for bootstrapping.
    * *pool_to_nih_\**: Map from pool name to normalized instance hours,
      with ``None`` for non-pooled jobs and non-:py:mod:`mrjob` jobs.
    """
    s = {}  # stats for all clusters

    s['clusters'] = [_cluster_to_full_summary(cluster, now=now)
                     for cluster in clusters]

    # from here on out, we only process s['clusters']

    # total usage
    for nih_type in ('nih_billed', 'nih_used', 'nih_bbnu'):
        s[nih_type] = float(sum(
            cs[nih_type] for cs in s['clusters']))

    # break down by usage/waste
    s['bootstrap_nih_used'] = float(sum(
        cs['usage'][0]['nih_used'] for cs in s['clusters']
        if cs['usage']))
    s['job_nih_used'] = s['nih_used'] - s['bootstrap_nih_used']
    s['end_nih_bbnu'] = float(sum(
        cs['usage'][-1]['nih_bbnu'] for cs in s['clusters']
        if cs['usage']))
    s['other_nih_bbnu'] = s['nih_bbnu'] - s['end_nih_bbnu']

    # stats by date/hour
    for interval_type in ('date', 'hour'):
        for nih_type in ('nih_billed', 'nih_used', 'nih_bbnu'):
            key = '%s_to_%s' % (interval_type, nih_type)
            start_to_nih = {}
            for cs in s['clusters']:
                for u in cs['usage']:
                    for start, nih in u[key].items():
                        start_to_nih.setdefault(start, 0.0)
                        start_to_nih[start] += nih
            s[key] = start_to_nih

    # break out by label (usually script name) and owner (usually current user)
    for key in ('label', 'owner'):
        for nih_type in ('nih_used', 'nih_billed', 'nih_bbnu'):
            key_to_nih = {}
            for cs in s['clusters']:
                for u in cs['usage']:
                    key_to_nih.setdefault(u[key], 0.0)
                    key_to_nih[u[key]] += u[nih_type]
            s['%s_to_%s' % (key, nih_type)] = key_to_nih

    # break down by job step. separate out un-pooled jobs
    for nih_type in ('nih_used', 'nih_billed', 'nih_bbnu'):
        job_step_to_nih = {}
        job_step_to_nih_no_pool = {}
        for cs in s['clusters']:
            for u in cs['usage'][1:]:
                job_step = (u['label'], u['step_num'])
                job_step_to_nih.setdefault(job_step, 0.0)
                job_step_to_nih[job_step] += u[nih_type]
                if not cs['pool']:
                    job_step_to_nih_no_pool.setdefault(job_step, 0.0)
                    job_step_to_nih_no_pool[job_step] += u[nih_type]

            s['job_step_to_%s' % nih_type] = job_step_to_nih
            s['job_step_to_%s_no_pool' % nih_type] = job_step_to_nih_no_pool

    # break down by pool
    for nih_type in ('nih_used', 'nih_billed', 'nih_bbnu'):
        pool_to_nih = {}
        for cs in s['clusters']:
            pool_to_nih.setdefault(cs['pool'], 0.0)
            pool_to_nih[cs['pool']] += cs[nih_type]

        s['pool_to_%s' % nih_type] = pool_to_nih

    return s


def _cluster_to_full_summary(cluster, now=None):
    """Convert a cluster to a full summary for use in creating a report,
    including billing/usage information.

    :param cluster: a :py:mod:`boto3` cluster data structure
    :param now: the current UTC time, as a :py:class:`datetime.datetime`.
                Defaults to the current time.

    Returns a dictionary with the keys from
    :py:func:`cluster_to_basic_summary` plus:

    * *nih_billed*: total normalized instances hours billed for this cluster
    * *nih_used*: total normalized instance hours actually used for
      bootstrapping and running jobs.
    * *nih_bbnu*: total usage billed but not used (`nih_billed - nih_used`)
    * *usage*: job-specific usage information, returned by
      :py:func:`_cluster_to_usage_data`.
    """
    cs = _cluster_to_basic_summary(cluster, now=now)

    cs['usage'] = _cluster_to_usage_data(
        cluster, basic_summary=cs, now=now)

    # add up billing info
    cs['nih_billed'] = float(sum(u['nih_billed'] for u in cs['usage']))

    for nih_type in ('nih_used', 'nih_bbnu'):
        cs[nih_type] = float(sum(u[nih_type] for u in cs['usage']))

    return cs


def _cluster_to_basic_summary(cluster, now=None):
    """Extract fields such as creation time, owner, etc. from the cluster.

    :param cluster: a :py:mod:`boto3` cluster data structure
    :param now: the current UTC time, as a :py:class:`datetime.datetime`.
                Defaults to the current time.

    Returns a dictionary with the following keys. These will be ``None`` if the
    corresponding field in the cluster is unavailable.

    * *created*: UTC `datetime.datetime` that the cluster was created,
      or ``None``
    * *end*: UTC `datetime.datetime` that the cluster finished, or ``None``
    * *id*: cluster ID, or ``None`` (this should never happen)
    * *label*: The label for the cluster (usually the module name of the
      :py:class:`~mrjob.job.MRJob` script that started it), or
      ``None`` for non-:py:mod:`mrjob` clusters.
    * *name*: cluster name, or ``None`` (this should never happen)
    * *nih*: number of normalized instance hours cluster *would* use if it
      ran to the end of the next full hour (
    * *num_steps*: Number of steps in the cluster.
    * *owner*: The owner for the cluster (usually the user that started it),
      or ``None`` for non-:py:mod:`mrjob` clusters.
    * *pool*: pool name (e.g. ``'default'``) if the cluster is pooled,
      otherwise ``None``.
    * *ran*: How long the cluster ran, or has been running, as a
      :py:class:`datetime.timedelta`. This will be ``timedelta(0)`` if
      the cluster hasn't started.
    * *ready*: UTC `datetime.datetime` that the cluster finished
      bootstrapping, or ``None``
    * *state*: The cluster's state as a string (e.g. ``'RUNNING'``)
    """
    if now is None:
        now = _boto3_now()

    bcs = {}  # basic cluster summary to fill in

    bcs['id'] = cluster['Id']
    bcs['name'] = cluster['Name']

    Status = cluster['Status']
    Timeline = Status.get('Timeline', {})

    bcs['created'] = Timeline.get('CreationDateTime')
    bcs['ready'] = Timeline.get('ReadyDateTime')
    bcs['end'] = Timeline.get('EndDateTime')

    if bcs['created']:
        bcs['ran'] = (bcs['end'] or now) - bcs['created']
    else:
        bcs['ran'] = timedelta(0)

    bcs['state'] = Status.get('State')

    bcs['num_steps'] = len(cluster['Steps'])

    bcs['pool'] = _pool_name(cluster)

    m = _JOB_KEY_RE.match(bcs['name'] or '')
    if m:
        bcs['label'], bcs['owner'] = m.group(1), m.group(2)
    else:
        bcs['label'], bcs['owner'] = None, None

    bcs['nih'] = float(cluster.get('NormalizedInstanceHours', 0))

    return bcs


def _cluster_to_usage_data(cluster, basic_summary=None, now=None):
    r"""Break billing/usage information for a cluster down by job.

    :param cluster: a :py:mod:`boto3` cluster data structure
    :param basic_summary: a basic summary of the cluster, returned by
                          :py:func:`_cluster_to_basic_summary`. If this
                          is ``None``, we'll call
                          :py:func:`_cluster_to_basic_summary` ourselves.
    :param now: the current UTC time, as a :py:class:`datetime.datetime`.
                Defaults to the current time.

    Returns a list of dictionaries containing usage information, one for
    bootstrapping, and one for each step that ran or is currently running. If
    the cluster hasn't started yet, return ``[]``.

    Usage dictionaries have the following keys:

    * *end*: when the job finished running, or *now* if it's still running.
    * *end_billing*: the effective end of the job for billing purposes, either
      when the next job starts, the current time if the job
      is still running, or the end of the next full hour
      in the cluster.
    * *nih_billed*: normalized instances hours billed for this job or
      bootstrapping step
    * *nih_used*: normalized instance hours actually used for running
      the job or bootstrapping
    * *nih_bbnu*: usage billed but not used (`nih_billed - nih_used`)
    * *date_to_nih_\**: map from a :py:class:`datetime.date` to number
      of normalized instance hours billed/used/billed but not used on that date
    * *hour_to_nih_\**: map from a :py:class:`datetime.datetime` to number
      of normalized instance hours billed/used/billed but not used during
      the hour starting at that time
    * *label*: job's label (usually the module name of the job), or for the
      bootstrapping step, the label of the cluster
    * *owner*: job's owner (usually the user that started it), or for the
      bootstrapping step, the owner of the cluster
    * *start*: when the job or bootstrapping step started, as a
      :py:class:`datetime.datetime`
    """
    bcs = basic_summary or _cluster_to_basic_summary(cluster)

    if now is None:
        now = _boto3_now()

    if not bcs['created']:
        return []

    # EMR no longer bills by the full hour, but NormalizedInstanceHours
    # still works that way
    full_hours = math.ceil(timedelta.total_seconds(bcs['ran']) / 60.0 / 60.0)
    nih_per_sec = bcs['nih'] / (full_hours * 3600.0)

    # EMR bills by the full second, and at least one minute per cluster
    cluster_end_billing = bcs['created'] + max(
        _round_up_to_next_second(bcs['ran']), timedelta(minutes=1))

    intervals = []

    # make a fake step for cluster startup and bootstrapping, so we don't
    # consider that wasted.
    intervals.append({
        'label': bcs['label'],
        'owner': bcs['owner'],
        'start': bcs['created'],
        'end': bcs['ready'] or bcs['end'] or now,
        'step_num': None,
    })

    for step in cluster['Steps']:
        Status = step['Status']
        Timeline = Status.get('Timeline', {})

        # we've reached the last step that's actually run
        if not Timeline.get('StartDateTime'):
            break

        step_start = Timeline['StartDateTime']

        step_end = Timeline.get('EndDateTime')
        if step_end is None:
            # step started running and was cancelled. credit it for 0 usage
            if bcs['end']:
                step_end = step_start
            # step is still running
            else:
                step_end = now

        m = _STEP_NAME_RE.match(step['Name'])
        if m:
            step_label = m.group(1)
            step_owner = m.group(2)
            step_num = int(m.group(6))
        else:
            step_label, step_owner, step_num = None, None, None

        intervals.append({
            'label': step_label,
            'owner': step_owner,
            'start': step_start,
            'end': step_end,
            'step_num': step_num,
        })

    # fill in end_billing
    for i in range(len(intervals) - 1):
        intervals[i]['end_billing'] = intervals[i + 1]['start']

    intervals[-1]['end_billing'] = cluster_end_billing

    # fill normalized usage information
    for interval in intervals:

        interval['nih_used'] = (
            nih_per_sec *
            timedelta.total_seconds(interval['end'] - interval['start']))

        interval['date_to_nih_used'] = dict(
            (d, nih_per_sec * secs)
            for d, secs
            in _subdivide_interval_by_date(interval['start'],
                                           interval['end']).items())

        interval['hour_to_nih_used'] = dict(
            (d, nih_per_sec * secs)
            for d, secs
            in _subdivide_interval_by_hour(interval['start'],
                                           interval['end']).items())

        interval['nih_billed'] = (
            nih_per_sec * timedelta.total_seconds(
                interval['end_billing'] - interval['start']))

        interval['date_to_nih_billed'] = dict(
            (d, nih_per_sec * secs)
            for d, secs
            in _subdivide_interval_by_date(interval['start'],
                                           interval['end_billing']).items())

        interval['hour_to_nih_billed'] = dict(
            (d, nih_per_sec * secs)
            for d, secs
            in _subdivide_interval_by_hour(interval['start'],
                                           interval['end_billing']).items())

        # time billed but not used
        interval['nih_bbnu'] = interval['nih_billed'] - interval['nih_used']

        interval['date_to_nih_bbnu'] = {}
        for d, nih_billed in interval['date_to_nih_billed'].items():
            nih_bbnu = nih_billed - interval['date_to_nih_used'].get(d, 0.0)
            if nih_bbnu:
                interval['date_to_nih_bbnu'][d] = nih_bbnu

        interval['hour_to_nih_bbnu'] = {}
        for d, nih_billed in interval['hour_to_nih_billed'].items():
            nih_bbnu = nih_billed - interval['hour_to_nih_used'].get(d, 0.0)
            if nih_bbnu:
                interval['hour_to_nih_bbnu'][d] = nih_bbnu

    return intervals


def _subdivide_interval_by_date(start, end):
    """Convert a time interval to a map from :py:class:`datetime.date` to
    the number of seconds within the interval on that date.

    *start* and *end* are :py:class:`datetime.datetime` objects.
    """
    if start.date() == end.date():
        date_to_secs = {start.date(): timedelta.total_seconds(end - start)}
    else:
        date_to_secs = {}

        date_to_secs[start.date()] = timedelta.total_seconds(
            datetime(start.year, start.month, start.day, tzinfo=start.tzinfo) +
            timedelta(days=1) - start)

        date_to_secs[end.date()] = timedelta.total_seconds(
            end - datetime(end.year, end.month, end.day, tzinfo=end.tzinfo))

        # fill in dates in the middle
        cur_date = start.date() + timedelta(days=1)
        while cur_date < end.date():
            date_to_secs[cur_date] = timedelta.total_seconds(timedelta(days=1))
            cur_date += timedelta(days=1)

    # remove zeros
    date_to_secs = dict(
        (d, secs) for d, secs in date_to_secs.items() if secs)

    return date_to_secs


def _subdivide_interval_by_hour(start, end):
    """Convert a time interval to a map from hours (represented as
    :py:class:`datetime.datetime` for the start of the hour) to the number of
    seconds during that hour that are within the interval

    *start* and *end* are :py:class:`datetime.datetime` objects.
    """
    start_hour = start.replace(minute=0, second=0, microsecond=0)
    end_hour = end.replace(minute=0, second=0, microsecond=0)

    if start_hour == end_hour:
        hour_to_secs = {start_hour: timedelta.total_seconds(end - start)}
    else:
        hour_to_secs = {}

        hour_to_secs[start_hour] = timedelta.total_seconds(
            start_hour + timedelta(hours=1) - start)

        hour_to_secs[end_hour] = timedelta.total_seconds(end - end_hour)

        # fill in dates in the middle
        cur_hour = start_hour + timedelta(hours=1)
        while cur_hour < end_hour:
            hour_to_secs[cur_hour] = timedelta.total_seconds(
                timedelta(hours=1))
            cur_hour += timedelta(hours=1)

    # remove zeros
    hour_to_secs = dict(
        (h, secs) for h, secs in hour_to_secs.items() if secs)

    return hour_to_secs


def _yield_clusters(max_days_ago=None, now=None, **runner_kwargs):
    """Get relevant cluster information from EMR.

    :param float max_days_ago: If set, don't fetch clusters created longer
                               than this many days ago.
    :param now: the current UTC time, as a :py:class:`datetime.datetime`.
                Defaults to the current time.
    :param runner_kwargs: keyword args to pass through to
                          :py:class:`~mrjob.emr.EMRJobRunner`
    """
    if now is None:
        now = _boto3_now()

    emr_client = EMRJobRunner(**runner_kwargs).make_emr_client()

    # if --max-days-ago is set, only look at recent jobs
    created_after = None
    if max_days_ago is not None:
        created_after = now - timedelta(days=max_days_ago)

    # use _DELAY to sleep 1 second after each API call (see #1091). Could
    # implement some sort of connection wrapper for this if it becomes more
    # generally useful.
    list_clusters_kwargs = dict(_delay=_DELAY)
    if created_after is not None:
        list_clusters_kwargs['CreatedAfter'] = created_after

    for cluster_summary in _boto3_paginate(
            'Clusters', emr_client, 'list_clusters', **list_clusters_kwargs):

        cluster_id = cluster_summary['Id']

        cluster = emr_client.describe_cluster(ClusterId=cluster_id)['Cluster']
        sleep(_DELAY)

        cluster['Steps'] = list(reversed(list(_boto3_paginate(
            'Steps', emr_client, 'list_steps',
            ClusterId=cluster_id, _delay=_DELAY))))

        yield cluster


def _print_report(stats, now=None):
    """Print final report.

    :param stats: a dictionary returned by :py:func:`_clusters_to_stats`
    :param now: the current UTC time, as a :py:class:`datetime.datetime`.
                Defaults to the current time.
    """
    if now is None:
        now = _boto3_now()

    s = stats

    if not s['clusters']:
        print('No clusters created in the past two months!')
        return

    print('Total  # of Clusters: %d' % len(s['clusters']))
    print()

    print('* All times are in UTC.')
    print()

    print('Min create time: %s' % min(cs['created'] for cs in s['clusters']))
    print('Max create time: %s' % max(cs['created'] for cs in s['clusters']))
    print('   Current time: %s' % now.replace(microsecond=0))
    print()

    print('* All usage is measured in Normalized Instance Hours, which are')
    print('  roughly equivalent to running an m1.medium instance for an hour.')
    print("  Billing is estimated, and may not match Amazon's system exactly.")
    print()

    # total compute-unit hours used
    def with_pct(usage):
        return (usage, _percent(usage, s['nih_billed']))

    print('Total billed:  %9.2f  %5.1f%%' % with_pct(s['nih_billed']))
    print('  Total used:  %9.2f  %5.1f%%' % with_pct(s['nih_used']))
    print('    bootstrap: %9.2f  %5.1f%%' % with_pct(s['bootstrap_nih_used']))
    print('    jobs:      %9.2f  %5.1f%%' % with_pct(s['job_nih_used']))
    print('  Total waste: %9.2f  %5.1f%%' % with_pct(s['nih_bbnu']))
    print('    at end:    %9.2f  %5.1f%%' % with_pct(s['end_nih_bbnu']))
    print('    other:     %9.2f  %5.1f%%' % with_pct(s['other_nih_bbnu']))
    print()

    if s['date_to_nih_billed']:
        print('Daily statistics:')
        print()
        print(' date          billed      used     waste   % waste')
        d = max(s['date_to_nih_billed'])
        while d >= min(s['date_to_nih_billed']):
            print(' %10s %9.2f %9.2f %9.2f     %5.1f' % (
                d,
                s['date_to_nih_billed'].get(d, 0.0),
                s['date_to_nih_used'].get(d, 0.0),
                s['date_to_nih_bbnu'].get(d, 0.0),
                _percent(s['date_to_nih_bbnu'].get(d, 0.0),
                         s['date_to_nih_billed'].get(d, 0.0))))
            d -= timedelta(days=1)
        print()

    if s['hour_to_nih_billed']:
        print('Hourly statistics:')
        print()
        print(' hour              billed      used     waste   % waste')
        h = max(s['hour_to_nih_billed'])
        while h >= min(s['hour_to_nih_billed']):
            print(' %13s  %9.2f %9.2f %9.2f     %5.1f' % (
                h.strftime('%Y-%m-%d %H'),
                s['hour_to_nih_billed'].get(h, 0.0),
                s['hour_to_nih_used'].get(h, 0.0),
                s['hour_to_nih_bbnu'].get(h, 0.0),
                _percent(s['hour_to_nih_bbnu'].get(h, 0.0),
                         s['hour_to_nih_billed'].get(h, 0.0))))
            h -= timedelta(hours=1)
        print()

    print('* clusters are considered to belong to the user and job that')
    print('  started them or last ran on them.')
    print()

    # Top jobs
    print('Top jobs, by total time used:')
    for label, nih_used in sorted(s['label_to_nih_used'].items(),
                                  key=lambda lb_nih: (-lb_nih[1], lb_nih[0])):
        print('  %9.2f %s' % (nih_used, label))
    print()

    print('Top jobs, by time billed but not used:')
    for label, nih_bbnu in sorted(
            s['label_to_nih_bbnu'].items(),
            key=lambda lb_nih1: (-lb_nih1[1], lb_nih1[0])):
        print('  %9.2f %s' % (nih_bbnu, label))
    print()

    # Top users
    print('Top users, by total time used:')
    for owner, nih_used in sorted(s['owner_to_nih_used'].items(),
                                  key=lambda o_nih: (-o_nih[1], o_nih[0])):
        print('  %9.2f %s' % (nih_used, owner))
    print()

    print('Top users, by time billed but not used:')
    for owner, nih_bbnu in sorted(s['owner_to_nih_bbnu'].items(),
                                  key=lambda o_nih2: (-o_nih2[1], o_nih2[0])):
        print('  %9.2f %s' % (nih_bbnu, owner))
    print()

    # Top job steps
    print('Top job steps, by total time used (step number first):')
    for (label, step_num), nih_used in sorted(
            s['job_step_to_nih_used'].items(),
            key=lambda k_nih: (-k_nih[1], k_nih[0])):

        if label:
            print('  %9.2f %3d %s' % (nih_used, step_num, label))
        else:
            print('  %9.2f     (non-mrjob step)' % (nih_used,))
    print()

    print('Top job steps, by total time billed but not used (un-pooled only):')
    for (label, step_num), nih_bbnu in sorted(
            s['job_step_to_nih_bbnu_no_pool'].items(),
            key=lambda k_nih3: (-k_nih3[1], k_nih3[0])):

        if label:
            print('  %9.2f %3d %s' % (nih_bbnu, step_num, label))
        else:
            print('  %9.2f     (non-mrjob step)' % (nih_bbnu,))
    print()

    # Top pools
    print('All pools, by total time billed:')
    for pool, nih_billed in sorted(s['pool_to_nih_billed'].items(),
                                   key=lambda p_nih: (-p_nih[1], p_nih[0])):
        print('  %9.2f %s' % (nih_billed, pool or '(not pooled)'))
    print()

    print('All pools, by total time billed but not used:')
    for pool, nih_bbnu in sorted(s['pool_to_nih_bbnu'].items(),
                                 key=lambda p_nih4: (-p_nih4[1], p_nih4[0])):
        print('  %9.2f %s' % (nih_bbnu, pool or '(not pooled)'))
    print()

    # Top clusters
    print('All clusters, by total time billed:')
    top_clusters = sorted(s['clusters'],
                          key=lambda cs: (-cs['nih_billed'], cs['name']))
    for cs in top_clusters:
        print('  %9.2f %-15s %s' % (
            cs['nih_billed'], cs['id'], cs['name']))
    print()

    print('All clusters, by time billed but not used:')
    top_clusters_bbnu = sorted(
        s['clusters'], key=lambda cs: (-cs['nih_bbnu'], cs['name']))
    for cs in top_clusters_bbnu:
        print('  %9.2f %-15s %s' % (
            cs['nih_bbnu'], cs['id'], cs['name']))
    print()

    # Details
    print('Details for all clusters:')
    print()
    print(' id              state                  created             steps'
          '        time ran     billed    waste   user   name')

    all_clusters = sorted(s['clusters'], key=lambda cs: cs['created'],
                          reverse=True)

    for cs in all_clusters:
        print(' %-15s %-22s %19s %3d %17s %9.2f %9.2f %8s %s' % (
            cs['id'], cs['state'], cs['created'], cs['num_steps'],
            strip_microseconds(cs['ran']), cs['nih_used'], cs['nih_bbnu'],
            (cs['owner'] or ''), (cs['label'] or ('not started by mrjob'))))


def _percent(x, total, default=0.0):
    """Return what percentage *x* is of *total*, or *default* if
    *total* is zero."""
    if total:
        return 100.0 * x / total
    else:
        return default


def _round_up_to_next_second(td):
    """Round up to the next second because that's how EMR bills."""
    if td.microseconds:
        return strip_microseconds(td) + timedelta(seconds=1)
    else:
        return td


if __name__ == '__main__':
    main()
