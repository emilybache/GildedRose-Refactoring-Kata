# -*- coding: utf-8 -*-
# Copyright 2009-2016 Yelp and Contributors
# Copyright 2017 Yelp
# Copyright 2018 Yelp, Google, Inc., and Contributors
# Copyright 2019 Yelp
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
"""Functions to populate py:class:`~argparse.ArgumentParser``
objects with categorized command line parameters.
"""
from __future__ import print_function

import json
import re
from argparse import Action
from argparse import ArgumentParser
from argparse import SUPPRESS
from logging import getLogger

from mrjob.conf import combine_cmds
from mrjob.conf import combine_dicts
from mrjob.conf import combine_envs
from mrjob.conf import combine_jobconfs
from mrjob.conf import combine_lists
from mrjob.conf import combine_paths
from mrjob.conf import combine_path_lists
from mrjob.parse import _parse_port_range_list
from mrjob.util import shlex_split

log = getLogger(__name__)

#: cleanup options:
#:
#: * ``'ALL'``: delete logs and local and remote temp files; stop cluster
#:   if on EMR and the job is not done when cleanup is run.
#: * ``'CLOUD_TMP'``: delete temp files on cloud storage (e.g. S3) only
#: * ``'CLUSTER'``: terminate the cluster if on EMR and the job is not done
#:    on cleanup
#: * ``'HADOOP_TMP'``: delete temp files on HDFS only
#: * ``'JOB'``: stop job if on EMR and the job is not done when cleanup runs
#: * ``'LOCAL_TMP'``: delete local temp files only
#: * ``'LOGS'``: delete logs only
#: * ``'NONE'``: delete nothing
#: * ``'TMP'``: delete local, HDFS, and cloud storage temp files, but not logs
CLEANUP_CHOICES = [
    'ALL',
    'CLOUD_TMP',
    'CLUSTER',
    'HADOOP_TMP',
    'JOB',
    'LOCAL_TMP',
    'LOGS',
    'NONE',
    'TMP',
]

# use to identify malformed JSON
_PROBABLY_JSON_RE = re.compile(r'^\s*[\{\[\"].*$')

# names of runners
_RUNNER_ALIASES = {
    'dataproc',
    'emr',
    'hadoop',
    'inline',
    'local',
    'spark',
}


### custom actions ###

def _default_to(namespace, dest, value):
    """Helper function; set the given attribute to *value* if it's None."""
    if getattr(namespace, dest) is None:
        setattr(namespace, dest, value)


# these actions are only used by _add_runner_args(), so we can assume *value*
# is a string

class _KeyValueAction(Action):
    """action for KEY=VALUE pairs"""
    # used for --cmdenv, --jobconf, and more

    def __call__(self, parser, namespace, value, option_string=None):
        try:
            k, v = value.split('=', 1)
        except ValueError:
            parser.error('%s argument %r is not of the form KEY=VALUE' % (
                option_string, value))

        _default_to(namespace, self.dest, {})
        getattr(namespace, self.dest)[k] = v


class _KeyNoneValueAction(Action):
    """action to set KEY to None"""
    def __call__(self, parser, namespace, value, option_string=None):
        _default_to(namespace, self.dest, {})
        getattr(namespace, self.dest)[value] = None


class _CleanupAction(Action):
    """action to parse a comma-separated list of cleanup constants."""

    def __call__(self, parser, namespace, value, option_string=None):
        result = []

        for choice in value.split(','):
            if choice in CLEANUP_CHOICES:
                result.append(choice)
            else:
                parser.error(
                    '%s got %s, which is not one of: %s' %
                    (option_string, choice, ', '.join(CLEANUP_CHOICES)))

        if 'NONE' in result and len(set(result)) > 1:
            parser.error(
                '%s: Cannot clean up both nothing and something!' %
                option_string)

        setattr(namespace, self.dest, result)


class _CommaSeparatedListAction(Action):
    """action to parse a comma-separated list of subnets.

    This eliminates whitespace
    """
    def __call__(self, parser, namespace, value, option_string=None):
        items = [s.strip() for s in value.split(',') if s]

        setattr(namespace, self.dest, items)


class _AppendCommaSeparatedItemsAction(Action):
    """action to parse a comma-separated list and append
    each of them to an existing list.

    This eliminates whitespace
    """
    def __call__(self, parser, namespace, value, option_string=None):
        _default_to(namespace, self.dest, [])

        items = [s.strip() for s in value.split(',') if s]

        getattr(namespace, self.dest).extend(items)


class _AppendArgsAction(Action):
    """action to parse one or more arguments and append them to a list."""
    def __call__(self, parser, namespace, value, option_string=None):
        _default_to(namespace, self.dest, [])

        args = shlex_split(value)

        getattr(namespace, self.dest).extend(args)


class _AppendJSONAction(Action):
    """action to parse JSON and append it to a list."""
    def __call__(self, parser, namespace, value, option_string=None):
        _default_to(namespace, self.dest, [])

        try:
            j = json.loads(value)
        except ValueError as e:
            parser.error('Malformed JSON passed to %s: %s' % (
                option_string, str(e)))

        getattr(namespace, self.dest).append(j)


class _KeyJSONValueAction(Action):
    """action for KEY=<json> pairs. Allows value to be a string, as long
    as it doesn't start with ``[``, ``{``, or ``"``."""
    # used for --extra-cluster-param

    def __call__(self, parser, namespace, value, option_string=None):
        try:
            k, v = value.split('=', 1)
        except ValueError:
            parser.error('%s argument %r is not of the form KEY=VALUE' % (
                option_string, value))

        try:
            v = json.loads(v)
        except ValueError:
            if _PROBABLY_JSON_RE.match(v):
                parser.error('%s argument %r is not valid JSON' % (
                    option_string, value))

        _default_to(namespace, self.dest, {})
        getattr(namespace, self.dest)[k] = v


class _JSONAction(Action):
    """action to parse a JSON"""

    def __call__(self, parser, namespace, value, option_string=None):
        try:
            j = json.loads(value)
        except ValueError as e:
            parser.error('Malformed JSON passed to %s: %s' % (
                option_string, str(e)))

        setattr(namespace, self.dest, j)


class _PortRangeAction(Action):
    """action to parse --ssh-bind-ports"""

    def __call__(self, parser, namespace, value, option_string=None):
        try:
            ports = _parse_port_range_list(value)
        except ValueError as e:
            parser.error('%s: invalid port range list %r: \n%s' %
                         (option_string, value, e.args[0]))

        setattr(namespace, self.dest, ports)


### mux opts ###

# these are used by MRJob to determine what part of a job to run
#
# this just maps dest to the args and kwargs to ArgumentParser.add_argument
# (minus the dest keyword arg)
_STEP_OPTS = dict(
    run_combiner=(
        ['--combiner'],
        dict(
            action='store_true',
            help='run a combiner',
        ),
    ),
    run_mapper=(
        ['--mapper'],
        dict(
            action='store_true',
            help='run a mapper'
        ),
    ),
    run_reducer=(
        ['--reducer'],
        dict(
            action='store_true',
            help='run a reducer',
        ),
    ),
    run_spark=(
        ['--spark'],
        dict(
            action='store_true',
            help='run Spark code',
        ),
    ),
    step_num=(
        ['--step-num'],
        dict(
            type=int,
            default=0,
            help='which step to execute (default is 0)',
        ),
    ),
)

# don't show these unless someone types --help -v --deprecated
_DEPRECATED_STEP_OPTS = set()  # none at the moment

# don't show these unless someone types --help --deprecated
_DEPRECATED_NON_RUNNER_OPTS = {'deprecated'}

### runner opts ###

# map from runner option name to dict with the following keys (all optional):
# cloud_role:
#   'connect' if needed when interacting with cloud services at all
#   'launch' if needed when creating a new cluster
#   (cloud runner options with no cloud role are only needed when running jobs)
# combiner: combiner func from mrjob.conf used to combine option values.
#   (if left blank, we use combine_values())
# deprecated: if true, this option is deprecated and slated for removal
# deprecated_aliases: list of old names for this option slated for removal
# switches: list of switches to add to ArgumentParser for this option. Items
#   have the format (['--switch-names', ...], dict(**kwargs)), where kwargs
#   can be:
#     action -- action to pass to add_argument() (e.g. 'store_true')
#     deprecated -- if True, this switch is deprecated and slated for removal
#     deprecated_aliases -- list of old '--switch-names' slated for removal
#     help -- help string to pass to add_argument()
#     type -- option type for add_argument() to enforce (e.g. float).
#   You can't set the ArgumentParser's default; we use [] if *action* is
#   'append' and None otherwise.
#
# the list of which options apply to which runner is in the runner class
# itself (e.g. EMRJobRunner.OPT_NAMES)
_RUNNER_OPTS = dict(
    add_steps_in_batch=dict(
        switches=[
            (['--add-steps-in-batch'], dict(
                action='store_true',
                help='For multi-step jobs, submit all steps at once',
            )),
            (['--no-add-steps-in-batch'], dict(
                action='store_false',
                help=('For multi-step jobs, submit steps successively after'
                      ' the previous one completes'),
            )),
        ]
    ),
    additional_emr_info=dict(
        cloud_role='launch',
        switches=[
            (['--additional-emr-info'], dict(
                help='A JSON string for selecting additional features on EMR',
            )),
        ],
    ),
    applications=dict(
        cloud_role='launch',
        combiner=combine_lists,
        switches=[
            (['--applications', '--application'], dict(
                action=_AppendCommaSeparatedItemsAction,
                help=('Additional applications to run on 4.x and 5.x'
                      ' AMIs, separated by commas (e.g.'
                      ' "Ganglia,Spark")'),
            )),
        ],
    ),
    aws_access_key_id=dict(
        cloud_role='connect',
    ),
    aws_secret_access_key=dict(
        cloud_role='connect',
    ),
    aws_session_token=dict(
        cloud_role='connect',
    ),
    bootstrap=dict(
        cloud_role='launch',
        combiner=combine_lists,
        switches=[
            (['--bootstrap'], dict(
                action='append',
                help=('A shell command to set up libraries etc. before any'
                      ' steps (e.g. "sudo apt-get -qy install python3"). You'
                      ' may interpolate files available via URL or locally'
                      ' with Hadoop Distributed Cache syntax'
                      ' ("sudo yum install -y foo.rpm#")'),
            )),
        ],
    ),
    bootstrap_actions=dict(
        cloud_role='launch',
        combiner=combine_lists,
        switches=[
            (['--bootstrap-action'], dict(
                action='append',
                help=('Raw bootstrap action scripts to run before any of the'
                      ' other bootstrap steps. You can use --bootstrap-action'
                      ' more than once. Local scripts will be automatically'
                      ' uploaded to S3. To add arguments, just use quotes:'
                      ' "foo.sh arg1 arg2"'),
            )),
        ],
    ),
    bootstrap_mrjob=dict(
        cloud_role='launch',
        switches=[
            (['--bootstrap-mrjob'], dict(
                action='store_true',
                help=("Automatically zip up the mrjob library and install it"
                      " when we run the mrjob. This is the default. Use"
                      " --no-bootstrap-mrjob if you've already installed"
                      " mrjob on your Hadoop cluster."),
            )),
            (['--no-bootstrap-mrjob'], dict(
                action='store_false',
                help=("Don't automatically zip up the mrjob library and"
                      " install it when we run this job. Use this if you've"
                      " already installed mrjob on your Hadoop cluster."),
            )),
        ],
    ),
    bootstrap_python=dict(
        cloud_role='launch',
        switches=[
            (['--bootstrap-python'], dict(
                action='store_true',
                help=('Attempt to install a compatible version of Python'
                      ' at bootstrap time. Currently this only does anything'
                      ' for Python 3, for which it is enabled by default.'),
            )),
            (['--no-bootstrap-python'], dict(
                action='store_false',
                help=("Don't automatically try to install a compatible version"
                      " of Python at bootstrap time."),
            )),
        ],
    ),
    bootstrap_spark=dict(
        cloud_role='launch',
        switches=[
            (['--bootstrap-spark'], dict(
                action='store_true',
                help="Auto-install Spark on the cluster (even if not needed)."
            )),
            (['--no-bootstrap-spark'], dict(
                action='store_false',
                help="Don't auto-install Spark on the cluster."
            )),
        ],
    ),
    check_input_paths=dict(
        switches=[
            (['--check-input-paths'], dict(
                action='store_true',
                help='Check input paths exist before running (the default)',
            )),
            (['--no-check-input-paths'], dict(
                action='store_false',
                help='Skip the checks to ensure all input paths exist',
            )),
        ],
    ),
    check_cluster_every=dict(
        switches=[
            (['--check-cluster-every'], dict(
                help=('How often (in seconds) to check status of your'
                      ' job/cluster'),
                type=float,
            )),
        ],
    ),
    cleanup=dict(
        switches=[
            (['--cleanup'], dict(
                action=_CleanupAction,
                help=('Comma-separated list of which directories to delete'
                      ' when a job succeeds, e.g. TMP,LOGS. Choices:'
                      ' %s (default: ALL)' % ', '.join(CLEANUP_CHOICES)),
            )),
        ],
    ),
    cleanup_on_failure=dict(
        switches=[
            (['--cleanup-on-failure'], dict(
                action=_CleanupAction,
                help=('Comma-separated list of which directories to delete'
                      ' when a job fails, e.g. TMP,LOGS. Choices:'
                      ' %s (default: NONE)' % ', '.join(CLEANUP_CHOICES)),
            )),
        ],
    ),
    cloud_fs_sync_secs=dict(
        cloud_role='launch',
        switches=[
            (['--cloud-fs-sync-secs'], dict(
                help=('How long to wait for remote FS to reach eventual'
                      ' consistency. This'
                      ' is typically less than a second but the'
                      ' default is 5.0 to be safe.'),
                type=float,
            )),
        ],
    ),
    cloud_log_dir=dict(
        cloud_role='launch',
        combiner=combine_paths,
        switches=[
            (['--cloud-log-dir'], dict(
                help='URI on remote FS to write logs into',
            )),
        ],
    ),
    cloud_tmp_dir=dict(
        cloud_role='launch',
        combiner=combine_paths,
        switches=[
            (['--cloud-tmp-dir'], dict(
                help='URI on remote FS to use as our temp directory.',
            )),
        ],
    ),
    cloud_part_size_mb=dict(
        cloud_role='launch',
        deprecated_aliases=['cloud_upload_part_size'],
        switches=[
            (['--cloud-part-size-mb'], dict(
                deprecated_aliases=['--cloud-upload-part-size'],
                help=('Upload files to cloud FS in parts no bigger than this'
                      ' many megabytes. Default is 100 MiB. Set to 0 to'
                      ' disable multipart uploading entirely.'),
                type=float,
            )),
        ],
    ),
    cluster_id=dict(
        switches=[
            (['--cluster-id'], dict(
                help='ID of an existing cluster to run our job on',
            )),
        ],
    ),
    cluster_properties=dict(
        cloud_role='launch',
        combiner=combine_dicts,
        switches=[
            (['--cluster-property'], dict(
                action=_KeyValueAction,
                help=('Properties to set in Hadoop config files on Dataproc.'
                      'Args take the form file_prefix:property=value.'
                      ' You can use --cluster-property multiple times.'
                      ' For more info, see'
                      ' https://cloud.google.com/dataproc/docs/concepts'
                      '/configuring-clusters/cluster-properties'),
            )),
        ],
    ),
    cmdenv=dict(
        combiner=combine_envs,
        switches=[
            (['--cmdenv'], dict(
                action=_KeyValueAction,
                help=('Set an environment variable for your job inside Hadoop '
                      'streaming/Spark. Must take the form KEY=VALUE.'
                      ' You can use --cmdenv multiple times.'),
            )),
        ],
    ),
    core_instance_config=dict(
        cloud_role='launch',
        switches=[
            (['--core-instance-config'], dict(
                action=_JSONAction,
                help=('detailed JSON dict of configs for the core'
                      ' (worker) instances'
                      ' on Dataproc, including disk config. For format, see'
                      ' https://cloud.google.com/dataproc/docs/reference/rest'
                      '/v1/projects.regions.clusters#InstanceGroupConfig'
                      ' (except that fields in your JSON should use'
                      ' snake_case, not camelCase).')
            )),
        ],
    ),
    core_instance_bid_price=dict(
        cloud_role='launch',
        switches=[
            (['--core-instance-bid-price'], dict(
                help=('Bid price to specify for core nodes when'
                      ' setting them up as EC2 spot instances (you probably'
                      ' only want to do this for task instances).'),
            )),
        ],
    ),
    core_instance_type=dict(
        cloud_role='launch',
        switches=[
            (['--core-instance-type'], dict(
                help='Type of GCE/EC2 core instance(s) to launch',
            )),
        ],
    ),
    docker_client_config=dict(
        switches=[
            (['--docker-client-config'], dict(
                help='Patch of docker client config, used connect to ECR',
            )),
        ],
    ),
    docker_image=dict(
        switches=[
            (['--docker-image'], dict(
                help='ID of Docker image to run tasks inside',
            )),
            (['--no-docker'], dict(
                action='store_const',
                const='',
                help="Don't run tasks inside Docker",
            )),
        ],
    ),
    docker_mounts=dict(
        combiner=combine_lists,
        switches=[
            (['--docker-mount'], dict(
                action='append',
                help=('Volume to mount into docker, e.g. '
                      ' "/etc/passwd:/etc/passwd:ro". May be used multiple'
                      ' times'),
            )),
        ]
    ),
    ebs_root_volume_gb=dict(
        cloud_role='launch',
        switches=[
            (['--ebs-root-volume-gb'], dict(
                help=('Size of root EBS volume, in GiB. Must be an integer.'
                      'Set to 0 to use the default'),
                type=int,
            )),
        ],
    ),
    ec2_endpoint=dict(
        cloud_role='connect',
        switches=[
            (['--ec2-endpoint'], dict(
                help=('Force mrjob to connect to EC2 on this endpoint'
                      ' (e.g. ec2.us-west-1.amazonaws.com).'
                      ' Default is to infer this from region.'),
            )),
        ],
    ),
    ec2_key_pair=dict(
        cloud_role='launch',
        switches=[
            (['--ec2-key-pair'], dict(
                help='Name of the SSH key pair you set up for EMR',
            )),
        ],
    ),
    ec2_key_pair_file=dict(
        combiner=combine_paths,
        switches=[
            (['--ec2-key-pair-file'], dict(
                help='Path to file containing SSH key for EMR',
            )),
        ],
    ),
    emr_action_on_failure=dict(
        cloud_role='launch',
        switches=[
            (['--emr-action-on-failure'], dict(
                help=('Action to take when a step fails'
                      ' (e.g. TERMINATE_CLUSTER, CANCEL_AND_WAIT, CONTINUE)'),
            )),
        ],
    ),
    emr_configurations=dict(
        cloud_role='launch',
        combiner=combine_lists,
        switches=[
            (['--emr-configuration'], dict(
                action=_AppendJSONAction,
                help=('Configuration to use on 4.x AMIs as a JSON-encoded'
                      ' dict; see'
                      ' http://docs.aws.amazon.com/ElasticMapReduce/latest/'
                      'ReleaseGuide/emr-configure-apps.html for examples'),
            )),
        ],
    ),
    emr_endpoint=dict(
        cloud_role='connect',
        switches=[
            (['--emr-endpoint'], dict(
                help=('Force mrjob to connect to EMR on this endpoint'
                      ' (e.g. us-west-1.elasticmapreduce.amazonaws.com).'
                      ' Default is to infer this from region.'),
            )),
        ],
    ),
    emulate_map_input_file=dict(
        switches=[
            (['--emulate-map-input-file'], dict(
                action='store_true',
                help=("In the first mapper, set $mapreduce_map_input_file to"
                      " the input file path, like Hadoop would, to support"
                      " jobs that use"
                      " jobconf_from_env('mapreduce.map.input.file')."
                      " Ignored if job sets HADOOP_INPUT_FORMAT."),
            )),
            (['--no-emulate-map-input-file'], dict(
                action='store_false',
                help=("Disables setting $mapreduce_map_input_file"),
            )),
        ],
    ),
    enable_emr_debugging=dict(
        cloud_role='launch',
        switches=[
            (['--enable-emr-debugging'], dict(
                action='store_true',
                help='Enable storage of Hadoop logs in SimpleDB',
            )),
            (['--disable-emr-debugging'], dict(
                action='store_false',
                help=('Disable storage of Hadoop logs in SimpleDB (the'
                      ' default)'),
            )),
        ],
    ),
    extra_cluster_params=dict(
        cloud_role='launch',
        combiner=combine_dicts,
        switches=[
            (['--extra-cluster-param'], dict(
                action=_KeyJSONValueAction,
                help=('extra parameter to pass to cloud API when creating'
                      ' a cluster, to access features not currently supported'
                      ' by mrjob. Takes the form <param>=<value>, where value'
                      ' is JSON or a string. Use <param>=null to unset a'
                      ' parameter'),
            )),
        ],
    ),
    gcloud_bin=dict(
        combiner=combine_cmds,
        switches=[
            (['--gcloud-bin'], dict(help='path to gcloud binary')),
        ],
    ),
    gcs_region=dict(
        cloud_role='connect',
        switches=[
            (['--gcs-region'], dict(
                help='region to create Google Cloud Storage buckets in',
            )),
        ],
    ),
    hadoop_bin=dict(
        combiner=combine_cmds,
        switches=[
            (['--hadoop-bin'], dict(help='path to hadoop binary')),
        ],
    ),
    hadoop_extra_args=dict(
        combiner=combine_lists,
        switches=[
            (['--hadoop-args'], dict(
                action=_AppendArgsAction,
                help=('One or more arguments to pass to the hadoop binary.'
                      ' (e.g. --hadoop-args="-fs file:///").'),
            )),
        ],
    ),
    hadoop_log_dirs=dict(
        combiner=combine_path_lists,
        switches=[
            (['--hadoop-log-dirs'], dict(
                action='append',
                help=('Directory to search for hadoop logs in. You can use'
                      ' --hadoop-log-dir multiple times.'),
            )),
        ],
    ),
    hadoop_streaming_jar=dict(
        combiner=combine_paths,
        switches=[
            (['--hadoop-streaming-jar'], dict(
                help=('Path of your hadoop streaming jar (locally, or on'
                      ' S3/HDFS). In EMR, use a file:// URI to refer to a jar'
                      ' on the master node of your cluster.'),
            )),
        ],
    ),
    hadoop_tmp_dir=dict(
        combiner=combine_paths,
        switches=[
            (['--hadoop-tmp-dir'], dict(
                help='Temp space on HDFS (default is tmp/mrjob)',
            )),
        ],
    ),
    hadoop_version=dict(
        switches=[
            (['--hadoop-version'], dict(
                help='Specific version of Hadoop to simulate',
            )),
        ],
    ),
    iam_endpoint=dict(
        cloud_role='launch',  # not 'connect'; only used to create clusters
        switches=[
            (['--iam-endpoint'], dict(
                help=('Force mrjob to connect to IAM on this endpoint'
                      ' (e.g. iam.us-gov.amazonaws.com)'),
            )),
        ],
    ),
    iam_instance_profile=dict(
        cloud_role='launch',
        switches=[
            (['--iam-instance-profile'], dict(
                help=('EC2 instance profile to use for the EMR cluster -- see'
                      ' "Configure IAM Roles for Amazon EMR" in AWS docs'),
            )),
        ],
    ),
    iam_service_role=dict(
        cloud_role='launch',
        switches=[
            (['--iam-service-role'], dict(
                help=('IAM service role to use for the EMR cluster -- see'
                      ' "Configure IAM Roles for Amazon EMR" in AWS docs')
            )),
        ],
    ),
    image_id=dict(
        cloud_role='launch',
        switches=[
            (['--image-id'], dict(
                help='ID of custom AWS machine image (AMI) to use',
            )),
        ],
    ),
    image_version=dict(
        cloud_role='launch',
        switches=[
            (['--image-version'], dict(
                help='version of EMR/Dataproc machine image to run',
            )),
        ],
    ),
    instance_groups=dict(
        cloud_role='launch',
        switches=[
            (['--instance-groups'], dict(
                action=_JSONAction,
                help=('detailed JSON list of EMR instance configs, including'
                      ' EBS configuration. See docs for --instance-groups'
                      ' at http://docs.aws.amazon.com/cli/latest/reference'
                      '/emr/create-cluster.html'),
            )),
        ],
    ),
    instance_fleets=dict(
        cloud_role='launch',
        switches=[
            (['--instance-fleets'], dict(
                action=_JSONAction,
                help=('detailed JSON list of instance fleets, including'
                      ' EBS configuration. See docs for --instance-fleets'
                      ' at http://docs.aws.amazon.com/cli/latest/reference'
                      '/emr/create-cluster.html'),
            )),
        ],
    ),
    instance_type=dict(
        cloud_role='launch',
        switches=[
            (['--instance-type'], dict(
                help=('Type of GCE/EC2 instance(s) to launch \n'
                      ' GCE - e.g. n1-standard-1, n1-highcpu-4, n1-highmem-4'
                      ' -- See'
                      ' https://cloud.google.com/compute/docs/machine-types\n'
                      ' EC2 - e.g. m1.medium, c3.xlarge, r3.xlarge '
                      ' -- See http://aws.amazon.com/ec2/instance-types/'),
            )),
        ],
    ),
    jobconf=dict(
        combiner=combine_jobconfs,
        switches=[
            (['-D', '--jobconf'], dict(
                action=_KeyValueAction,
                help=('passed through to hadoop streaming as -D and to Spark'
                      ' as --conf. Should take the form KEY=VALUE'),
            )),
        ],
    ),
    label=dict(
        cloud_role='launch',
        switches=[
            (['--label'], dict(
                help='Alternate label for the job, to help us identify it.',
            )),
        ],
    ),
    libjars=dict(
        combiner=combine_path_lists,
        switches=[
            (['--libjars'], dict(
                action=_AppendCommaSeparatedItemsAction,
                help=('Paths of JARs to pass to Hadoop with -libjars,'
                      ' separated by commas. On EMR,'
                      ' these can also be URIs; use file:/// to'
                      ' reference JARs already on the EMR cluster.')
            )),
        ],
    ),
    local_tmp_dir=dict(
        combiner=combine_paths,
        switches=[
            (['--local-tmp-dir'], dict(
                help='temp directory on local filesystem',
            )),
        ],
    ),
    master_instance_bid_price=dict(
        cloud_role='launch',
        switches=[
            (['--master-instance-bid-price'], dict(
                help=('Bid price to specify for the master node when'
                      ' setting it up as an EC2 spot instance (you probably'
                      ' only want to do this for task instances).'),
            )),
        ],
    ),
    master_instance_config=dict(
        cloud_role='launch',
        switches=[
            (['--master-instance-config'], dict(
                action=_JSONAction,
                help=('detailed JSON dict of configs for the master instance'
                      ' on Dataproc including disk config. For format, see'
                      ' https://cloud.google.com/dataproc/docs/reference/rest'
                      '/v1/projects.regions.clusters#InstanceGroupConfig'
                      ' (except that fields in your JSON should use'
                      ' snake_case, not camelCase).')
            )),
        ],
    ),
    master_instance_type=dict(
        cloud_role='launch',
        switches=[
            (['--master-instance-type'], dict(
                help='Type of GCE/EC2 master instance to launch',
            )),
        ],
    ),
    max_clusters_in_pool=dict(
        switches=[
            (['--max-clusters-in-pool'], dict(
                help=("If attempting to join a pooled cluster, don't bail"
                      " out and create a new one if there are at least"
                      " this many clusters already in the pool."),
                type=int,
            )),
        ],
    ),
    max_concurrent_steps=dict(
        cloud_role='launch',
        switches=[
            (['--max-concurrent-steps'], dict(
                help=("Maximum number of steps that may run on the cluster"
                      " at one time. Default is 1"),
                type=int,
            )),
        ],
    ),
    max_mins_idle=dict(
        cloud_role='launch',
        switches=[
            (['--max-mins-idle'], dict(
                help=("If we create a cluster, have it automatically"
                      " terminate itself after it's been idle this many"
                      " minutes"),
                type=float,
            )),
        ],
    ),
    # Spark runner only, only passed in on the command line (see #2040)
    max_output_files=dict(
        switches=[
            (['--max-output-files'], dict(
                help=('Maximum number of output files when running a'
                      ' streaming job on Spark; just runs rdd.coalesce()'
                      ' before outputting files'),
                type=int,
            )),
        ],
    ),
    # min_available_* options aren't in the "launch" group because they
    # are only used when joining a cluster, not creating one
    min_available_mb=dict(
        switches=[
            (['--min-available-mb'], dict(
                help=('When attempting to join a pooled cluster, check'
                      ' the YARN resource manager to ensure at least'
                      ' this much memory is available.'),
                type=int,
            )),
        ]
    ),
    min_available_virtual_cores=dict(
        switches=[
            (['--min-available-virtual-cores'], dict(
                help=('When attempting to join a pooled cluster, check'
                      ' the YARN resource manager to ensure at least'
                      ' this much CPU capacity is available.'),
                type=float,
            )),
        ]
    ),
    network=dict(
        cloud_role='launch',
        switches=[
            (['--network'], dict(
                help=('URI of Google Compute Engine network to launch cluster'
                      " in. Can't be used with --subnet."),
            )),
        ],
    ),
    num_core_instances=dict(
        cloud_role='launch',
        switches=[
            (['--num-core-instances'], dict(
                help='Total number of core instances to launch',
                type=int,
            )),
        ],
    ),
    num_task_instances=dict(
        cloud_role='launch',
        switches=[
            (['--num-task-instances'], dict(
                help='Total number of task instances to launch',
                type=int,
            )),
        ],
    ),
    num_cores=dict(
        cloud_role='launch',
        switches=[
            (['--num-cores'], dict(
                help='Total number of core to use while running in local mode',
                type=int,
            )),
        ],
    ),
    owner=dict(
        cloud_role='launch',
        switches=[
            (['--owner'], dict(
                help='User who ran the job (default is the current user)',
            )),
        ],
    ),
    pool_clusters=dict(
        cloud_role='launch',
        switches=[
            (['--pool-clusters'], dict(
                action='store_true',
                help=('Add to an existing cluster or create a new one that'
                      ' does not terminate when the job completes.'),
            )),
            (['--no-pool-clusters'], dict(
                action='store_false',
                help="Don't run job on a pooled cluster (the default)",
            )),
        ],
    ),
    pool_jitter_seconds=dict(
        switches=[
            (['--pool-jitter-seconds'], dict(
                help=('If --max-pools-in-cluster is set, before launching a'
                      ' cluster, wait a random amount of time between 0 and'
                      ' this many seconds and then double-check the number'
                      ' of clusters in the pool before launching'),
                type=int,
            )),
        ],
    ),
    pool_name=dict(
        cloud_role='launch',
        switches=[
            (['--pool-name'], dict(
                help='Specify a pool name to join. Default is "default"',
            )),
        ],
    ),
    pool_timeout_minutes=dict(
        switches=[
            (['--pool-timeout-minutes'], dict(
                help=("If pooling can't join or create a cluster within this"
                      " many minutes, raise an exception. (0 means don't"
                      " timeout"),
                type=int,
            )),
        ],
    ),
    pool_wait_minutes=dict(
        switches=[
            (['--pool-wait-minutes'], dict(
                help=('Wait for a number of minutes for a cluster to finish'
                      ' if a job finishes, run job on its cluster. Otherwise'
                      " create a new one. (0, the default, means don't wait)"),
                type=int,
            )),
        ],
    ),
    project_id=dict(
        cloud_role='connect',
        deprecated_aliases=['gcp_project'],
        switches=[
            (['--project-id'], dict(
                deprecated_aliases=['--gcp-project'],
                help=('Project to use when connecting to Google Cloud Services'
                      ' and to run Cloud Dataproc jobs in')
            )),
        ],
    ),
    py_files=dict(
        combiner=combine_path_lists,
        switches=[
            (['--py-files'], dict(
                action=_AppendCommaSeparatedItemsAction,
                help=('.zip or .egg files to add to PYTHONPATH,'
                      ' separated by commas'),
            )),
        ],
    ),
    python_bin=dict(
        combiner=combine_cmds,
        switches=[
            (['--python-bin'], dict(
                help=('Alternate python command. You can include arguments,'
                      ' e.g. --python-bin "python -v"'),
            )),
        ],
    ),
    read_logs=dict(
        switches=[
            (['--read-logs'], dict(
                action='store_true',
                help=('Parse logs generated by the job to get counters and'
                      ' cause of error (the default).')
            )),
            (['--no-read-logs'], dict(
                action='store_false',
                help="Don't list or read logs generated by the job."
            )),
        ],
    ),
    region=dict(
        cloud_role='connect',
        switches=[
            (['--region'], dict(
                help='GCE/AWS region to run Dataproc/EMR jobs in.',
            )),
        ],
    ),
    release_label=dict(
        cloud_role='launch',
        switches=[
            (['--release-label'], dict(
                help=('Release Label (e.g. "emr-4.0.0"). Overrides'
                      ' --image-version'),
            )),
        ],
    ),
    s3_endpoint=dict(
        cloud_role='connect',
        switches=[
            (['--s3-endpoint'], dict(
                help=("Force mrjob to connect to S3 on this endpoint (e.g."
                      " s3-us-west-1.amazonaws.com). You usually shouldn't"
                      " set this; by default mrjob will choose the correct"
                      " endpoint for each S3 bucket based on its location."),
            )),
        ],
    ),
    s3_region=dict(
        cloud_role='connect',
        switches=[
            (['--s3-region'], dict(
                help='AWS region to create s3 buckets in',
            )),
        ],
    ),
    service_account=dict(
        cloud_role='launch',
        switches=[
            (['--service-account'], dict(
                help=('Service account to use when creating a Dataproc'
                      ' cluster. Usually takes the form'
                      ' [account_id]@[project_id].iam.gserviceaccount.com.'
                      ' Set to "" to use the default.'),
            )),
        ],
    ),
    service_account_scopes=dict(
        cloud_role='launch',
        switches=[
            (['--service-account-scopes'], dict(
                action=_CommaSeparatedListAction,
                help=("A comma-separated list of service account scopes"
                      " on Dataproc, used to limit your cluster's access."
                      " For each scope, you can specify the"
                      " full URI or just the name (e.g. 'logging.write')"),
            )),
        ],
    ),
    setup=dict(
        combiner=combine_lists,
        switches=[
            (['--setup'], dict(
                action='append',
                help=('A command to run before each mapper/reducer step in the'
                      ' shell ("touch foo"). You may interpolate files'
                      ' available via URL or on your local filesystem using'
                      ' Hadoop Distributed Cache syntax (". setup.sh#"). To'
                      ' interpolate archives, use #/: "cd foo.tar.gz#/; make'),
            )),
        ],
    ),
    sh_bin=dict(
        combiner=combine_cmds,
        switches=[
            (['--sh-bin'], dict(
                help=('Alternate shell command for setup scripts. You may'
                      ' include arguments, e.g. --sh-bin "bash -ex"'),
            )),
        ],
    ),
    skip_internal_protocol=dict(
        switches=[
            (['--skip-internal-protocol'], dict(
                action='store_true',
                help=("Don't use the job's internal protocol to communicate"
                      " between tasks internal to the job, instead relying"
                      " on Spark to encode and decode raw data structures.")
            )),
            (['--no-skip-internal-protocol'], dict(
                action='store_false',
                help='Use internal protocols as usual',
            )),
        ],
    ),
    sort_bin=dict(
        combiner=combine_cmds,
        switches=[
            (['--sort-bin'], dict(
                help=('Alternate shell command for the external sort binary.'
                      'You may include arguments, e.g. --sort-bin "sort -r"')
            )),
        ],
    ),
    spark_args=dict(
        combiner=combine_lists,
        switches=[
            (['--spark-args'], dict(
                action=_AppendArgsAction,
                help=('One or more arguments to pass to spark-submit'
                      ' (e.g. --spark-args="--properties-file my.conf").'),
            )),
        ],
    ),
    spark_deploy_mode=dict(
        switches=[
            (['--spark-deploy-mode'], dict(
                help=('--deploy-mode argument to spark-submit (e.g.'
                      ' "cluster". Default is "client"'),
            )),
        ]
    ),
    spark_master=dict(
        switches=[
            (['--spark-master'], dict(
                help=('--master argument to spark-submit (e.g. '
                      'spark://host:port, local). Default is "yarn"'),
            )),
        ],
    ),
    spark_submit_bin=dict(
        combiner=combine_cmds,
        switches=[
            (['--spark-submit-bin'], dict(
                help='spark-submit binary. You may include arguments.'
            )),
        ],
    ),
    spark_tmp_dir=dict(
        cloud_role='launch',
        combiner=combine_paths,
        switches=[
            (['--spark-tmp-dir'], dict(
                help=('optional URI visible to Spark executors to use as our'
                      ' temp directory.'),
            )),
        ],
    ),
    ssh_add_bin=dict(
        combiner=combine_cmds,
        switches=[
            (['--ssh-add-bin'], dict(
                help=("Name/path of ssh-add binary. Arguments are allowed"
                      " (e.g. --ssh-bin 'ssh-add -v')"),
            )),
        ],
    ),
    ssh_bin=dict(
        combiner=combine_cmds,
        switches=[
            (['--ssh-bin'], dict(
                help=("Name/path of ssh binary. Arguments are allowed (e.g."
                      " --ssh-bin 'ssh -v')"),
            )),
        ],
    ),
    ssh_bind_ports=dict(
        switches=[
            (['--ssh-bind-ports'], dict(
                action=_PortRangeAction,
                help=('A list of port ranges that are safe to listen on,'
                      ' delimited by colons and commas, with syntax like'
                      ' 2000[:2001][,2003,2005:2008,etc].'
                      ' Defaults to 40001:40840.'),
            )),
        ],
    ),
    ssh_tunnel=dict(
        switches=[
            (['--ssh-tunnel'], dict(
                action='store_true',
                help=('Open an SSH tunnel to the Hadoop job tracker/resource'
                      ' manager'),
            )),
            (['--no-ssh-tunnel'], dict(
                action='store_false',
                help=("Don't open an SSH tunnel to the Hadoop job"
                      " tracker/resource manager (the default)"),
            )),
        ],
    ),
    ssh_tunnel_is_open=dict(
        switches=[
            (['--ssh-tunnel-is-open'], dict(
                action='store_true',
                help=('Make ssh tunnel accessible from remote hosts (not just'
                      ' localhost)'),
            )),
            (['--ssh-tunnel-is-closed'], dict(
                action='store_false',
                help=('Make ssh tunnel accessible from localhost only (the'
                      ' default)'),
            )),
        ],
    ),
    subnet=dict(
        cloud_role='launch',
        switches=[
            (['--subnet'], dict(
                help=('ID of Amazon VPC subnet/URI of Google Compute Engine'
                      ' subnetwork to launch cluster in.'),
            )),
            (['--subnets'], dict(
                action=_CommaSeparatedListAction,
                help=('Like --subnet, but with a comma-separated list, to'
                      ' specify multiple subnets in conjunction with'
                      ' --instance-fleets (EMR only)'),
            )),
        ],
    ),
    tags=dict(
        cloud_role='launch',
        combiner=combine_dicts,
        switches=[
            (['--tag'], dict(
                action=_KeyValueAction,
                help=('Metadata tags to apply to the EMR cluster; '
                      'should take the form KEY=VALUE. You can use --tag '
                      'multiple times'),
            )),
        ],
    ),
    task_instance_bid_price=dict(
        cloud_role='launch',
        switches=[
            (['--task-instance-bid-price'], dict(
                help=('Bid price to specify for task nodes when'
                      ' setting them up as EC2 spot instances'),
            )),
        ],
    ),
    task_instance_config=dict(
        cloud_role='launch',
        switches=[
            (['--task-instance-config'], dict(
                action=_JSONAction,
                help=('detailed JSON dict of configs for the task'
                      ' (secondary worker) instances'
                      ' on Dataproc including disk config. For format, see'
                      ' https://cloud.google.com/dataproc/docs/reference/rest'
                      '/v1/projects.regions.clusters#InstanceGroupConfig'
                      ' (except that fields in your JSON should use'
                      ' snake_case, not camelCase).')
            )),
        ],
    ),
    task_instance_type=dict(
        cloud_role='launch',
        switches=[
            (['--task-instance-type'], dict(
                help='Type of GCE/EC2 task instance(s) to launch',
            )),
        ],
    ),
    task_python_bin=dict(
        combiner=combine_cmds,
        switches=[
            (['--task-python-bin'], dict(
                help=('Name/path of alternate python command to use to'
                      " run tasks (e.g. mappers); doesn't affect setup"
                      ' wrapper scripts. Defaults to'
                      ' current Python interpreter.'),
            )),
        ],
    ),
    upload_archives=dict(
        combiner=combine_path_lists,
        switches=[
            (['--archives'], dict(
                action=_AppendCommaSeparatedItemsAction,
                help=('Archives to unpack in the working directory of the'
                      ' script, separated by commas. Use "#" to assign a'
                      ' different name to each directory (e.g. '
                      '"foo-libs.zip#lib,bar.tar.gz#bar")'),
            )),
        ],
    ),
    upload_dirs=dict(
        combiner=combine_path_lists,
        switches=[
            (['--dirs'], dict(
                action=_AppendCommaSeparatedItemsAction,
                help=('Directories to tarball and unpack in the working'
                      ' directory of the script, separated by commas. Append'
                      '#<name> to each directory to assign a different name'
                      ' (e.g. "foo#lib,bar#local-bar")'),
            )),
        ],
    ),
    upload_files=dict(
        combiner=combine_path_lists,
        switches=[
            (['--files'], dict(
                action=_AppendCommaSeparatedItemsAction,
                help=('Files to copy to the working directory of the script,'
                      ' separated by commas. Use "#"'
                      ' to assign a different name to each file (e.g. '
                      '"foo.db#bar.db")'),
            )),
        ],
    ),
    zone=dict(
        cloud_role='launch',
        switches=[
            (['--zone'], dict(
                help=('GCE zone/AWS availability zone to run Dataproc/EMR jobs'
                      ' in.'),
            )),
        ],
    ),
)


def _combiners(opt_names, runner_alias=None):
    return {
        name: config['combiner']
        for name, config in _RUNNER_OPTS.items()
        if name in opt_names and 'combiner' in config
    }


def _deprecated_aliases(opt_names):
    results = {}

    for name, config in _RUNNER_OPTS.items():
        if name not in opt_names:
            continue

        if config.get('deprecated_aliases'):
            for alias in config['deprecated_aliases']:
                results[alias] = name

    return results


def _filter_by_role(opt_names, *cloud_roles):
    return {
        opt_name
        for opt_name, conf in _RUNNER_OPTS.items()
        if opt_name in opt_names and conf.get('cloud_role') in cloud_roles
    }


def _add_runner_args(parser, opt_names=None, include_deprecated=True,
                     customize_switches=None, suppress_switches=None):
    """add switches for the given runner opts to the given
    ArgumentParser, alphabetically by destination. If *opt_names* is
    None, include all runner opts."""
    if opt_names is None:
        opt_names = set(_RUNNER_OPTS)

    for opt_name in sorted(opt_names):
        _add_runner_args_for_opt(
            parser, opt_name,
            include_deprecated=include_deprecated,
            customize_switches=customize_switches,
            suppress_switches=suppress_switches
        )


def _add_runner_args_for_opt(parser, opt_name, include_deprecated=True,
                             customize_switches=None, suppress_switches=None):
    """Add switches for a single option (*opt_name*) to the given parser."""
    if customize_switches is None:
        customize_switches = {}

    if suppress_switches is None:
        suppress_switches = set()

    conf = _RUNNER_OPTS[opt_name]

    if conf.get('deprecated') and not include_deprecated:
        return

    switches = conf.get('switches') or []

    def suppressed(switches):
        return any(sw in suppress_switches for sw in switches)

    for args, kwargs in switches:
        kwargs = dict(kwargs)

        # allow customization
        for switch in args:
            if switch in customize_switches:
                kwargs.update(customize_switches[switch])

        deprecated_aliases = kwargs.pop('deprecated_aliases', None)
        deprecated = kwargs.pop('deprecated', False)

        # add this switch
        if (include_deprecated or not deprecated) and not suppressed(args):
            kwargs['dest'] = opt_name

            if kwargs.get('action') == 'append':
                kwargs['default'] = []
            else:
                kwargs['default'] = None

            parser.add_argument(*args, **kwargs)

        # add a switch for deprecated aliases
        if (deprecated_aliases and include_deprecated and
                not suppressed(deprecated_aliases)):
            help = 'Deprecated alias%s for %s' % (
                ('es' if len(deprecated_aliases) > 1 else ''),
                args[-1])
            parser.add_argument(
                *deprecated_aliases,
                **combine_dicts(kwargs, dict(help=help)))


### non-runner switches ###

def _add_basic_args(parser):
    """Switches for all command line tools"""

    parser.add_argument(
        '-c', '--conf-path', dest='conf_paths',
        action='append',
        help='Path to alternate mrjob.conf file to read from')

    parser.add_argument(
        '--no-conf', dest='conf_paths', action='store_const', const=[],
        help="Don't load mrjob.conf even if it's available")

    parser.add_argument(
        '-q', '--quiet', dest='quiet', default=None,
        action='store_true',
        help="Don't print anything to stderr")

    parser.add_argument(
        '-v', '--verbose', dest='verbose', default=None,
        action='store_true', help='print more messages to stderr')


def _add_job_args(parser, include_deprecated=True, include_steps=True):

    parser.add_argument(
        '--cat-output', dest='cat_output',
        default=None, action='store_true',
        help="Stream job output to stdout")

    parser.add_argument(
        '--no-cat-output', dest='cat_output',
        default=None, action='store_false',
        help="Don't stream job output to stdout")

    if include_deprecated:
        parser.add_argument(
            '--no-output', dest='cat_output',
            default=None, action='store_false',
            help='Deprecated alias for --no-cat-output')

    parser.add_argument(
        '-o', '--output-dir', dest='output_dir', default=None,
        help='Where to put final job output. This must be an s3:// URL ' +
        'for EMR, an HDFS path for Hadoop, and a system path for local,' +
        'and must be empty')

    parser.add_argument(
        '-r', '--runner', dest='runner',
        choices=sorted(_RUNNER_ALIASES),
        help=('Where to run the job; one of: %s' % ', '.join(
            sorted(_RUNNER_ALIASES))))

    parser.add_argument(
        '--step-output-dir', dest='step_output_dir', default=None,
        help=('A directory to store output from job steps other than'
              ' the last one. Useful for debugging. Currently'
              ' ignored by local runners.'))

    if include_deprecated:
        parser.add_argument(
            '--deprecated', dest='deprecated', action='store_true',
            help='include help for deprecated options')

    parser.add_argument(
        '-h', '--help', dest='help', action='store_true',
        help='show this message and exit')


def _add_step_args(parser, include_deprecated=False):
    """Add switches that determine what part of the job a MRJob runs."""
    for dest, (args, kwargs) in _STEP_OPTS.items():
        if dest in _DEPRECATED_STEP_OPTS and not include_deprecated:
            continue
        kwargs = dict(dest=dest, **kwargs)
        parser.add_argument(*args, **kwargs)


### other utilities for switches ###

def _print_help_for_runner(opt_names, include_deprecated=False):
    help_parser = ArgumentParser(usage=SUPPRESS, add_help=False)

    _add_runner_args(help_parser, opt_names,
                     include_deprecated=include_deprecated)

    help_parser.print_help()


def _print_basic_help(option_parser, usage, include_deprecated=False,
                      include_steps=False):
    """Print all help for the parser. Unlike similar functions, this needs a
    parser so that it can include custom options added by a
    :py:class:`~mrjob.job.MRJob`.
    """
    help_parser = ArgumentParser(usage=usage, add_help=False)

    _add_basic_args(help_parser)
    _add_job_args(help_parser, include_deprecated=include_deprecated,
                  include_steps=include_steps)

    basic_dests = {action.dest for action in help_parser._actions}

    # add other custom args added by the user
    for action in option_parser._actions:
        # option_parser already includes deprecated option dests

        # this excludes deprecated switch aliases (e.g. --no-output)
        if action.dest in basic_dests:
            continue

        # this excludes the --deprecated switch (which is explained below)
        if action.dest in _DEPRECATED_NON_RUNNER_OPTS:
            continue

        # this excludes options that are shown with --help -r <runner>
        if action.dest in _RUNNER_OPTS:
            continue

        # don't include steps if *include_steps* isn't set
        if action.dest in _STEP_OPTS and not include_steps:
            continue

        # this excludes the ARGS option, which is already covered by usage
        if not action.option_strings:
            continue

        # found a custom option. thanks, library user!
        help_parser._add_action(action)

    help_parser.print_help()

    print()
    print('To see help for a specific runner, use --help -r <runner name>')
    print()
    if not include_steps:
        print('To include switches that control what part of a job runs,'
              ' use --help -v')
        print()
    if not include_deprecated:
        print('To include help for deprecated options, add --deprecated')
        print()


def _parse_raw_args(parser, args):
    """Simulate parsing by *parser*, return a list of tuples of
    (dest, option_string, args).

    If *args* contains unknown args or is otherwise malformed, we don't
    raise an error (we leave this to the actual argument parser).
    """
    results = []

    class RawArgAction(Action):
        def __call__(self, parser, namespace, values, option_string=None):
            # ignore *namespace*, append to *results*
            results.append((self.dest, option_string, values))

    def error(msg):
        raise ValueError(msg)

    raw_parser = ArgumentParser(add_help=False)
    raw_parser.error = error

    for action in parser._actions:
        # single args become single item lists
        nargs = 1 if action.nargs is None else action.nargs

        raw_parser.add_argument(*action.option_strings,
                                action=RawArgAction,
                                dest=action.dest,
                                nargs=nargs)

    # leave errors to the real parser
    raw_parser.parse_known_args(args)

    return results


def _alphabetize_actions(arg_parser):
    """Alphabetize arg parser actions for the sake of nicer help printouts."""
    # based on https://stackoverflow.com/questions/12268602/sort-argparse-help-alphabetically  # noqa
    for g in arg_parser._action_groups:
        g._group_actions.sort(key=lambda opt: opt.dest)
