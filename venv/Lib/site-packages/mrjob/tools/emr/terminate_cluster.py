# Copyright 2009-2012 Yelp
# Copyright 2013 David Marin and Steve Johnson
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
"""Terminate an existing EMR cluster.

Usage::

    mrjob terminate-cluster [options] CLUSTER_ID

Terminate an existing EMR cluster.

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
  -q, --quiet           Don't print anything to stderr
  --region REGION       GCE/AWS region to run Dataproc/EMR jobs in.
  --s3-endpoint S3_ENDPOINT
                        Force mrjob to connect to S3 on this endpoint (e.g.
                        s3-us-west-1.amazonaws.com). You usually shouldn't set
                        this; by default mrjob will choose the correct
                        endpoint for each S3 bucket based on its location.
  -t, --test            Don't actually delete any files; just log that we
                        would
  -v, --verbose         print more messages to stderr
"""
import logging
from argparse import ArgumentParser

from mrjob.emr import EMRJobRunner
from mrjob.job import MRJob
from mrjob.options import _add_basic_args
from mrjob.options import _add_runner_args
from mrjob.options import _alphabetize_actions
from mrjob.options import _filter_by_role

log = logging.getLogger(__name__)


def main(cl_args=None):
    # parser command-line args
    arg_parser = _make_arg_parser()
    options = arg_parser.parse_args(cl_args)

    MRJob.set_up_logging(quiet=options.quiet, verbose=options.verbose)

    # create the persistent job
    runner = EMRJobRunner(**_runner_kwargs(options))
    log.debug('Terminating cluster %s' % options.cluster_id)
    runner.make_emr_client().terminate_job_flows(
        JobFlowIds=[options.cluster_id])
    log.info('Terminated cluster %s' % options.cluster_id)


def _make_arg_parser():
    usage = '%(prog)s terminate-cluster [options] CLUSTER_ID'
    description = 'Terminate an existing EMR cluster.'

    arg_parser = ArgumentParser(usage=usage, description=description)

    arg_parser.add_argument(
        '-t', '--test', dest='test', default=False,
        action='store_true',
        help="Don't actually delete any files; just log that we would")

    arg_parser.add_argument(
        dest='cluster_id',
        help='ID of cluster to terminate')

    _add_basic_args(arg_parser)
    _add_runner_args(
        arg_parser,
        _filter_by_role(EMRJobRunner.OPT_NAMES, 'connect'))

    _alphabetize_actions(arg_parser)

    return arg_parser


def _runner_kwargs(options):
    kwargs = options.__dict__.copy()
    for unused_arg in ('cluster_id', 'quiet', 'verbose', 'test'):
        del kwargs[unused_arg]

    return kwargs


if __name__ == '__main__':
    main()
