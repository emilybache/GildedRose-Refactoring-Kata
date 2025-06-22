# Copyright 2012 Yelp
# Copyright 2013 David Marin and Steve Johnson
# Copyright 2014 Brett Gibson
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
"""Report jobs running for more than a certain number of hours (by default,
24.0). This can help catch buggy jobs and Hadoop/EMR operational issues.

Suggested usage: run this as a daily cron job with the ``-q`` option::

    0 0 * * * mrjob report-long-jobs

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
  -x EXCLUDE, --exclude EXCLUDE
                        Exclude clusters that match the specified tags.
                        Specifed in the form TAG_KEY,TAG_VALUE.
  -h, --help            show this help message and exit
  --min-hours MIN_HOURS
                        Minimum number of hours a job can run before we report
                        it. Default: 24.0
  -q, --quiet           Don't print anything to stderr
  --region REGION       GCE/AWS region to run Dataproc/EMR jobs in.
  --s3-endpoint S3_ENDPOINT
                        Force mrjob to connect to S3 on this endpoint (e.g. s3
                        -us-west-1.amazonaws.com). You usually shouldn't set
                        this; by default mrjob will choose the correct
                        endpoint for each S3 bucket based on its location.
  -v, --verbose         print more messages to stderr
"""
from __future__ import print_function

from argparse import ArgumentParser
from datetime import timedelta
import logging

from mrjob.aws import _boto3_now
from mrjob.aws import _boto3_paginate
from mrjob.emr import EMRJobRunner
from mrjob.job import MRJob
from mrjob.options import _add_basic_args
from mrjob.options import _add_runner_args
from mrjob.options import _alphabetize_actions
from mrjob.options import _filter_by_role
from mrjob.util import strip_microseconds

# default minimum number of hours a job can run before we report it.
DEFAULT_MIN_HOURS = 24.0

log = logging.getLogger(__name__)


def main(args=None):
    now = _boto3_now()

    arg_parser = _make_arg_parser()
    options = arg_parser.parse_args(args)

    MRJob.set_up_logging(quiet=options.quiet, verbose=options.verbose)

    log.info('getting information about running jobs')

    min_time = timedelta(hours=options.min_hours)

    emr_client = EMRJobRunner(**_runner_kwargs(options)).make_emr_client()
    cluster_summaries = _boto3_paginate(
        'Clusters', emr_client, 'list_clusters',
        ClusterStates=['STARTING', 'BOOTSTRAPPING', 'RUNNING'])

    if not options.exclude:
        filtered_cluster_summaries = cluster_summaries
    else:
        filtered_cluster_summaries = _filter_clusters(
            cluster_summaries, emr_client, options.exclude)

    job_info = _find_long_running_jobs(
        emr_client, filtered_cluster_summaries, min_time, now=now)

    _print_report(job_info)


def _runner_kwargs(options):
    """Given the command line options, return the arguments to
    :py:class:`EMRJobRunner`
    """
    kwargs = options.__dict__.copy()
    for unused_arg in ('quiet', 'verbose', 'min_hours', 'exclude'):
        del kwargs[unused_arg]

    return kwargs


def _filter_clusters(cluster_summaries, emr_client, exclude_strings):
    """ Filter out clusters that have tags matching any specified in
    exclude_strings.
    :param cluster_summaries: a list of :py:mod:`boto3` cluster summary data
                              structures
    :param exclude_strings: A list of strings of the form TAG_KEY,TAG_VALUE
    """
    exclude_as_dicts = []
    for exclude_string in exclude_strings:
        exclude_key, exclude_value = exclude_string.split(',')
        exclude_as_dicts.append({'Key': exclude_key, 'Value': exclude_value})

    for cs in cluster_summaries:
        cluster_id = cs['Id']
        cluster_tags = emr_client.describe_cluster(
            ClusterId=cluster_id)['Cluster']['Tags']
        for cluster_tag in cluster_tags:
            if cluster_tag in exclude_as_dicts:
                break
        else:
            yield cs


def _find_long_running_jobs(emr_client, cluster_summaries, min_time, now=None):
    """Identify jobs that have been running or pending for a long time.

    :param clusters: a list of :py:mod:`boto3` cluster summary data structures
    :param min_time: a :py:class:`datetime.timedelta`: report jobs running or
                     pending longer than this
    :param now: the current UTC time, as a :py:class:`datetime.datetime`.
                Defaults to the current time.

    For each job that is running or pending longer than *min_time*, yields
    a dictionary with the following keys:

    * *cluster_id*: the cluster's unique ID (e.g. ``j-SOMECLUSTER``)
    * *name*: name of the step, or the cluster when bootstrapping
    * *state*: state of the step (``'RUNNING'`` or ``'PENDING'``) or, if there
               is no step, the cluster (``'STARTING'`` or ``'BOOTSTRAPPING'``)
    * *time*: amount of time step was running or pending, as a
              :py:class:`datetime.timedelta`
    """
    if now is None:
        now = _boto3_now()

    for cs in cluster_summaries:

        # special case for jobs that are taking a long time to bootstrap
        if cs['Status']['State'] in ('STARTING', 'BOOTSTRAPPING'):
            # there isn't a way to tell when the cluster stopped being
            # provisioned and started bootstrapping, so just measure
            # from cluster creation time
            created = cs['Status']['Timeline']['CreationDateTime']

            time_running = now - created

            if time_running >= min_time:
                yield({'cluster_id': cs['Id'],
                       'name': cs['Name'],
                       'state': cs['Status']['State'],
                       'time': time_running})

        # the default case: running clusters
        if cs['Status']['State'] != 'RUNNING':
            continue

        steps = list(reversed(list(_boto3_paginate(
            'Steps', emr_client, 'list_steps', ClusterId=cs['Id']))))

        running_steps = [
            step for step in steps if step['Status']['State'] == 'RUNNING']
        pending_steps = [
            step for step in steps if step['Status']['State'] == 'PENDING']

        if running_steps:
            # should be only one, but if not, we should know about it
            for step in running_steps:

                start = step['Status']['Timeline']['StartDateTime']

                time_running = now - start

                if time_running >= min_time:
                    yield({'cluster_id': cs['Id'],
                           'name': step['Name'],
                           'state': step['Status']['State'],
                           'time': time_running})

        # sometimes EMR says it's "RUNNING" but doesn't actually run steps!
        elif pending_steps:
            step = pending_steps[0]

            # PENDING job should have run starting when the cluster
            # became ready, or the previous step completed
            start = cs['Status']['Timeline']['ReadyDateTime']
            for step in steps:
                if step['Status']['State'] == 'COMPLETED':
                    start = step['Status']['Timeline']['EndDateTime']

            time_pending = now - start

            if time_pending >= min_time:
                yield({'cluster_id': cs['Id'],
                       'name': step['Name'],
                       'state': step['Status']['State'],
                       'time': time_pending})


def _print_report(job_info):
    """Takes in a dictionary of info about a long-running job (see
    :py:func:`_find_long_running_jobs`), and prints information about it
    on a single (long) line.
    """
    for ji in job_info:
        print('%-15s %13s for %17s (%s)' % (
            ji['cluster_id'],
            ji['state'], _format_timedelta(ji['time']),
            ji['name']))


def _format_timedelta(time):
    """Format a timedelta for use in a columnar format. This just
    tweaks stuff like ``'3 days, 9:00:00'`` to line up with
    ``'3 days, 10:00:00'``
    """
    result = str(strip_microseconds(time))

    parts = result.split()
    if len(parts) == 3 and len(parts[-1]) == 7:
        return '%s %s  %s' % tuple(parts)
    else:
        return result


def _make_arg_parser():
    usage = '%(prog)s report-long-jobs [options]'
    description = ('Report jobs running for more than a certain number of'
                   ' hours (by default, %.1f). This can help catch buggy jobs'
                   ' and Hadoop/EMR operational issues.' % DEFAULT_MIN_HOURS)

    arg_parser = ArgumentParser(usage=usage, description=description)

    arg_parser.add_argument(
        '--min-hours', dest='min_hours', type=float,
        default=DEFAULT_MIN_HOURS,
        help=('Minimum number of hours a job can run before we report it.'
              ' Default: %(default)s'))

    arg_parser.add_argument(
        '-x', '--exclude', action='append',
        help=('Exclude clusters that match the specified tags.'
              ' Specifed in the form TAG_KEY,TAG_VALUE.')
    )

    _add_basic_args(arg_parser)
    _add_runner_args(
        arg_parser,
        _filter_by_role(EMRJobRunner.OPT_NAMES, 'connect')
    )

    _alphabetize_actions(arg_parser)

    return arg_parser


if __name__ == '__main__':
    main()
