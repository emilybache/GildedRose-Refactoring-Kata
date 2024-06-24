# -*- coding: utf-8 -*-
# Copyright 2009-2012 Yelp
# Copyright 2013 Yelp and Contributors
# Copyright 2014 Brett Gibson
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
# limitations under the License
"""Terminate idle EMR clusters that meet the criteria passed in on the command
line (or, by default, clusters that have been idle for one hour).

Suggested usage: run this as a cron job with the ``-q`` option::

    */30 * * * * mrjob terminate-idle-clusters -q

.. versionchanged:: 0.6.4

   Skips termination-protected idle clusters, rather than crashing. (This was
   also backported to mrjob v0.5.12.)

Options::

  -c CONF_PATHS, --conf-path CONF_PATHS
                        Path to alternate mrjob.conf file to read from
  --no-conf             Don't load mrjob.conf even if it's available
  --dry-run             Don't actually kill idle jobs; just log that we would
  --ec2-endpoint EC2_ENDPOINT
                        Force mrjob to connect to EC2 on this endpoint (e.g.
                        ec2.us-west-1.amazonaws.com). Default is to infer this
                        from region.
  --emr-endpoint EMR_ENDPOINT
                        Force mrjob to connect to EMR on this endpoint (e.g.
                        us-west-1.elasticmapreduce.amazonaws.com). Default is
                        to infer this from region.
  -h, --help            show this help message and exit
  --max-mins-idle MAX_MINS_IDLE
                        Max number of minutes a cluster can go without
                        bootstrapping, running a step, or having a new step
                        created. This will fire even if there are pending
                        steps which EMR has failed to start. Make sure you set
                        this higher than the amount of time your jobs can take
                        to start instances and bootstrap.
  --max-mins-locked MAX_MINS_LOCKED
                        Deprecated, does nothing
  --pool-name POOL_NAME
                        Only terminate clusters in the given named pool.
  --pooled-only         Only terminate pooled clusters
  -q, --quiet           Don't print anything to stderr
  --region REGION       GCE/AWS region to run Dataproc/EMR jobs in.
  --s3-endpoint S3_ENDPOINT
                        Force mrjob to connect to S3 on this endpoint (e.g.
                        s3-us-west-1.amazonaws.com). You usually shouldn't set
                        this; by default mrjob will choose the correct
                        endpoint for each S3 bucket based on its location.
  --unpooled-only       Only terminate un-pooled clusters
  -v, --verbose         print more messages to stderr
"""
from __future__ import print_function

import logging
from argparse import ArgumentParser
from datetime import timedelta

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

log = logging.getLogger(__name__)

_DEFAULT_MAX_MINS_IDLE = 60


def main(cl_args=None):
    arg_parser = _make_arg_parser()
    options = arg_parser.parse_args(cl_args)

    MRJob.set_up_logging(quiet=options.quiet,
                         verbose=options.verbose)

    max_mins_idle = options.max_mins_idle

    _maybe_terminate_clusters(
        dry_run=options.dry_run,
        max_mins_idle=max_mins_idle,
        unpooled_only=options.unpooled_only,
        now=_boto3_now(),
        pool_name=options.pool_name,
        pooled_only=options.pooled_only,
        quiet=options.quiet,
        **_runner_kwargs(options)
    )


def _runner_kwargs(options):
    kwargs = options.__dict__.copy()
    for unused_arg in ('quiet', 'verbose', 'max_mins_idle',
                       'unpooled_only', 'pooled_only', 'pool_name', 'dry_run'):
        del kwargs[unused_arg]

    return kwargs


def _maybe_terminate_clusters(dry_run=False,
                              max_mins_idle=None,
                              now=None,
                              pool_name=None,
                              pooled_only=False,
                              unpooled_only=False,
                              quiet=False,
                              **kwargs):
    if now is None:
        now = _boto3_now()

    # old default behavior
    if max_mins_idle is None:
        max_mins_idle = _DEFAULT_MAX_MINS_IDLE

    runner = EMRJobRunner(**kwargs)
    emr_client = runner.make_emr_client()

    num_starting = 0
    num_bootstrapping = 0
    num_done = 0
    num_idle = 0
    num_pending = 0
    num_running = 0

    # include RUNNING to catch clusters with PENDING jobs that
    # never ran (see #365).
    for cluster_summary in _boto3_paginate(
            'Clusters', emr_client, 'list_clusters',
            ClusterStates=['WAITING', 'RUNNING']):

        cluster_id = cluster_summary['Id']

        # check if cluster is done
        if _is_cluster_done(cluster_summary):
            num_done += 1
            continue

        # check if cluster is starting
        if _is_cluster_starting(cluster_summary):
            num_starting += 1
            continue

        # check if cluster is bootstrapping
        if _is_cluster_bootstrapping(cluster_summary):
            num_bootstrapping += 1
            continue

        # need steps to learn more about cluster
        steps = list(reversed(list(_boto3_paginate(
            'Steps', emr_client, 'list_steps',
            ClusterId=cluster_id))))

        if any(_is_step_running(step) for step in steps):
            num_running += 1
            continue

        # cluster is idle
        time_idle = now - _time_last_active(cluster_summary, steps)
        is_pending = _cluster_has_pending_steps(steps)

        # need to get actual cluster to see tags
        cluster = emr_client.describe_cluster(ClusterId=cluster_id)['Cluster']

        pool = _pool_name(cluster)

        if is_pending:
            num_pending += 1
        else:
            num_idle += 1

        log.debug(
            'cluster %s %s for %s, %s (%s) - %s' %
            (cluster_id,
             'pending' if is_pending else 'idle',
             strip_microseconds(time_idle),
             ('unpooled' if pool is None else 'in %s pool' % pool),
             cluster_summary['Name'],
             'protected' if cluster['TerminationProtected'] else 'unprotected',
             ))

        # filter out clusters that don't meet our criteria
        if (max_mins_idle is not None and
                time_idle <= timedelta(minutes=max_mins_idle)):
            continue

        if (pooled_only and pool is None):
            continue

        if (unpooled_only and pool is not None):
            continue

        if (pool_name is not None and pool != pool_name):
            continue

        if cluster['TerminationProtected']:
            continue

        # terminate idle cluster
        _terminate_and_notify(
            runner=runner,
            cluster_id=cluster_id,
            cluster_name=cluster_summary['Name'],
            num_steps=len(steps),
            is_pending=is_pending,
            time_idle=time_idle,
            dry_run=dry_run,
            quiet=quiet)

    log.info(
        'Cluster statuses: %d starting, %d bootstrapping, %d running,'
        ' %d pending, %d idle, %d done' % (
            num_starting, num_bootstrapping, num_running,
            num_pending, num_idle, num_done))


def _is_cluster_done(cluster):
    """Return True if the given cluster is done running."""
    return bool(cluster['Status']['State'] == 'TERMINATING' or
                cluster['Status']['Timeline'].get('EndDateTime'))


def _is_cluster_starting(cluster_summary):
    return cluster_summary['Status']['State'] == 'STARTING'


def _is_cluster_bootstrapping(cluster_summary):
    """Return ``True`` if *cluster_summary* is currently bootstrapping."""
    return (cluster_summary['Status']['State'] != 'STARTING' and
            not cluster_summary['Status']['Timeline'].get('ReadyDateTime'))


def _is_cluster_running(steps):
    return any(_is_step_running(step) for step in steps)


def _is_step_running(step):
    """Return true if the given step is currently running."""
    return (step['Status']['State'] not in
            ('CANCELLED', 'INTERRUPTED') and
            step['Status']['Timeline'].get('StartDateTime') and
            not step['Status']['Timeline'].get('EndDateTime'))


def _cluster_has_pending_steps(steps):
    """Does *cluster* have any steps in the ``PENDING`` state?"""
    return any(step['Status']['State'] == 'PENDING' for step in steps)


def _time_last_active(cluster_summary, steps):
    """When did something last happen with the given cluster?

    Things we look at:

    * cluster's ``CreationDateTime`` (always set)
    * cluster's ``ReadyDateTime`` (i.e. when bootstrapping finished)
    * step's ``CreationDateTime`` for any step
    * step's ``StartDateTime`` for any step
    * step's ``EndDateTime`` for any step

    This is not really meant to be run on clusters which are currently
    running, or done.
    """
    timestamps = []

    for key in 'CreationDateTime', 'ReadyDateTime':
        value = cluster_summary['Status']['Timeline'].get(key)
        if value:
            timestamps.append(value)

    for step in steps:
        for key in 'CreationDateTime', 'StartDateTime', 'EndDateTime':
            value = step['Status']['Timeline'].get(key)
            if value:
                timestamps.append(value)

    return max(timestamps)


def _terminate_and_notify(runner, cluster_id, cluster_name, num_steps,
                          is_pending, time_idle,
                          dry_run=False, quiet=False):
    emr_client = runner.make_emr_client()

    if not dry_run:
        emr_client.terminate_job_flows(JobFlowIds=[cluster_id])

    if not quiet:
        fmt = ('Terminated cluster %s (%s); was %s for %s')
        msg = fmt % (
            cluster_id, cluster_name,
            'pending' if is_pending else 'idle',
            strip_microseconds(time_idle))

        print(msg)


def _make_arg_parser():
    usage = '%(prog)s terminate-idle-clusters [options]'
    description = ('Terminate idle EMR clusters that meet the criteria'
                   ' passed in on the command line (or, by default,'
                   ' clusters that have been idle for one hour).')

    arg_parser = ArgumentParser(usage=usage, description=description)

    arg_parser.add_argument(
        '--max-mins-idle', dest='max_mins_idle',
        default=None, type=float,
        help=('Max number of minutes a cluster can go without bootstrapping,'
              ' running a step, or having a new step created. This will fire'
              ' even if there are pending steps which EMR has failed to'
              ' start. Make sure you set this higher than the amount of time'
              ' your jobs can take to start instances and bootstrap.'))
    arg_parser.add_argument(
        '--max-mins-locked', dest='max_mins_locked',
        type=float,
        help='Deprecated, does nothing')
    arg_parser.add_argument(
        '--unpooled-only', dest='unpooled_only', action='store_true',
        default=False,
        help='Only terminate un-pooled clusters')
    arg_parser.add_argument(
        '--pooled-only', dest='pooled_only', action='store_true',
        default=False,
        help='Only terminate pooled clusters')
    arg_parser.add_argument(
        '--pool-name', dest='pool_name', default=None,
        help='Only terminate clusters in the given named pool.')
    arg_parser.add_argument(
        '--dry-run', dest='dry_run', default=False,
        action='store_true',
        help="Don't actually kill idle jobs; just log that we would")

    _add_basic_args(arg_parser)
    _add_runner_args(
        arg_parser,
        _filter_by_role(EMRJobRunner.OPT_NAMES, 'connect'))

    _alphabetize_actions(arg_parser)

    return arg_parser


if __name__ == '__main__':
    main()
