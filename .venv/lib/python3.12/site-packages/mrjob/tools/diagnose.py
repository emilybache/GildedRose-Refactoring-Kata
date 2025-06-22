# Copyright 2017-2018 Yelp
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
"""Print probable cause of error for a failed step.

Currently this only works on EMR.

Usage::

    mrjob diagnose [opts] j-CLUSTERID

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
                        Force mrjob to connect to S3 on this endpoint (e.g. s3
                        -us-west-1.amazonaws.com). You usually shouldn't set
                        this; by default mrjob will choose the correct
                        endpoint for each S3 bucket based on its location.
  --step-id STEP_ID     ID of a particular failed step to diagnose
  -v, --verbose         print more messages to stderr

.. versionadded:: 0.6.1
"""
from argparse import ArgumentParser
from logging import getLogger

from mrjob.aws import _boto3_paginate
from mrjob.emr import EMRJobRunner
from mrjob.job import MRJob
from mrjob.logs.errors import _format_error
from mrjob.options import _add_basic_args
from mrjob.options import _add_runner_args
from mrjob.options import _alphabetize_actions
from mrjob.options import _filter_by_role

log = getLogger(__name__)


def main(cl_args=None):
    arg_parser = _make_arg_parser()
    options = arg_parser.parse_args(cl_args)

    MRJob.set_up_logging(quiet=options.quiet, verbose=options.verbose)

    runner_kwargs = {k: v for k, v in options.__dict__.items()
                     if k not in ('quiet', 'verbose', 'step_id')}

    runner = EMRJobRunner(**runner_kwargs)
    emr_client = runner.make_emr_client()

    # pick step
    step = _get_step(emr_client, options.cluster_id, options.step_id)

    if not step:
        raise SystemExit(1)

    if step['Status']['State'] != 'FAILED':
        log.warning('step %s has state %s, not FAILED' %
                    (step['Id'], step['Status']['State']))

    # interpret logs
    log.info('Diagnosing step %s (%s)' % (step['Id'], step['Name']))

    log_interpretation = dict(step_id=step['Id'])

    step_type = _infer_step_type(step)

    error = runner._pick_error(log_interpretation, step_type)

    # print error
    if error:
        log.error('Probable cause of failure:\n\n%s\n\n' %
                  _format_error(error))
    else:
        log.warning('No error detected')


def _get_step(emr_client, cluster_id, step_id=None):

    # just iterate backwards through steps, rather than filtering
    # by step ID or status. usually it'll be the last step anyhow

    for step in _boto3_paginate('Steps', emr_client, 'list_steps',
                                ClusterId=cluster_id):

        if _step_matches(step, step_id=step_id):
            return step
    else:
        if step_id:
            log.error('step %s not found on cluster %s' %
                      (step_id, cluster_id))
        else:
            log.error('cluster %s has no failed steps' % cluster_id)


def _step_matches(step, step_id=None):
    if not step_id:
        return step['Status']['State'] == 'FAILED'
    else:
        return step['Id'] == step_id


def _infer_step_type(step):
    args = step['Config']['Args']

    # all that matters for log parsing is picking out Spark steps
    # (doesn't matter if it's spark or spark_jar or spark_script)
    #
    # and of course we don't know the logging habits of jar steps,
    # so we might as well use streaming's logic
    if '--master' in args and '--deploy-mode' in args:
        return 'spark'
    else:
        return 'streaming'


def _make_arg_parser():
    usage = '%(prog)s diagnose [opts] [--step-id STEP_ID] CLUSTER_ID'
    description = (
        'Get probable cause of failure for step on CLUSTER_ID.'
        ' By default we look at the last failed step')
    arg_parser = ArgumentParser(usage=usage, description=description)

    _add_basic_args(arg_parser)
    _add_runner_args(
        arg_parser,
        _filter_by_role(EMRJobRunner.OPT_NAMES, 'connect'))

    arg_parser.add_argument(
        dest='cluster_id',
        help='ID of cluster with failed step')
    arg_parser.add_argument(
        '--step-id', dest='step_id',
        help='ID of a particular failed step to diagnose')

    _alphabetize_actions(arg_parser)

    return arg_parser


if __name__ == '__main__':
    main()
