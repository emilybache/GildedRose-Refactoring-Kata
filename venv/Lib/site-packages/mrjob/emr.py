# -*- coding: utf-8 -*-
# Copyright 2009-2017 Yelp and Contributors
# Copyright 2018 Yelp
# Copyright 2019 Yelp and Contributors
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
import hashlib
import json
import logging
import os
import os.path
import pipes
import posixpath
import re
import time
from collections import OrderedDict
from collections import defaultdict
from datetime import datetime
from datetime import timedelta
from math import ceil
from random import randint

try:
    import botocore.client
    import botocore.exceptions
    botocore  # quiet "redefinition of unused ..." warning from pyflakes
except ImportError:
    botocore = None

try:
    import boto3
    boto3  # quiet "redefinition of unused ..." warning from pyflakes
except ImportError:
    # don't require boto3; MRJobs don't actually need it when running
    # inside hadoop streaming
    boto3 = None


import mrjob
import mrjob.step
from mrjob.aws import _DEFAULT_AWS_REGION
from mrjob.aws import EC2_INSTANCE_TYPE_TO_MEMORY
from mrjob.aws import _boto3_now
from mrjob.aws import _boto3_paginate
from mrjob.aws import _wrap_aws_client
from mrjob.cloud import HadoopInTheCloudJobRunner
from mrjob.compat import map_version
from mrjob.compat import version_gte
from mrjob.conf import combine_dicts
from mrjob.fs.composite import CompositeFilesystem
from mrjob.fs.hadoop import HadoopFilesystem
from mrjob.fs.local import LocalFilesystem
from mrjob.fs.s3 import S3Filesystem
from mrjob.fs.s3 import _client_error_status
from mrjob.fs.s3 import _endpoint_url
from mrjob.fs.s3 import _get_bucket_region
from mrjob.fs.ssh import SSHFilesystem
from mrjob.hadoop import _DEFAULT_YARN_HDFS_LOG_DIR
from mrjob.iam import _FALLBACK_INSTANCE_PROFILE
from mrjob.iam import _FALLBACK_SERVICE_ROLE
from mrjob.iam import get_or_create_mrjob_instance_profile
from mrjob.iam import get_or_create_mrjob_service_role
from mrjob.logs.bootstrap import _check_for_nonzero_return_code
from mrjob.logs.bootstrap import _interpret_emr_bootstrap_stderr
from mrjob.logs.bootstrap import _ls_emr_bootstrap_stderr_logs
from mrjob.logs.counters import _pick_counters
from mrjob.logs.errors import _log_probable_cause_of_failure
from mrjob.logs.mixin import LogInterpretationMixin
from mrjob.logs.spark import _interpret_spark_logs
from mrjob.logs.step import _interpret_emr_step_stderr
from mrjob.logs.step import _interpret_emr_step_syslog
from mrjob.logs.step import _ls_emr_step_stderr_logs
from mrjob.logs.step import _ls_emr_step_syslogs
from mrjob.parse import is_s3_uri
from mrjob.parse import _parse_progress_from_job_tracker
from mrjob.parse import _parse_progress_from_resource_manager
from mrjob.pool import _attempt_to_lock_cluster
from mrjob.pool import _attempt_to_unlock_cluster
from mrjob.pool import _cluster_name_suffix
from mrjob.pool import _instance_fleets_satisfy
from mrjob.pool import _instance_groups_satisfy
from mrjob.pool import _parse_cluster_name_suffix
from mrjob.py2 import PY2
from mrjob.py2 import string_types
from mrjob.py2 import to_unicode
from mrjob.py2 import urljoin
from mrjob.py2 import urlopen
from mrjob.runner import _blank_out_conflicting_opts
from mrjob.setup import UploadDirManager
from mrjob.setup import WorkingDirManager
from mrjob.step import StepFailedException
from mrjob.step import _is_spark_step_type
from mrjob.util import shlex_split
from mrjob.util import strip_microseconds
from mrjob.util import random_identifier


log = logging.getLogger(__name__)

# how to set up the SSH tunnel for various AMI versions
_IMAGE_VERSION_TO_SSH_TUNNEL_CONFIG = {
    '2': dict(
        localhost=True,
        name='job tracker',
        path='/jobtracker.jsp',
        port=9100,
    ),
    '3': dict(
        localhost=False,
        name='resource manager',
        path='/cluster',
        port=9026,
    ),
    '4': dict(
        localhost=False,
        name='resource manager',
        path='/cluster',
        port=8088,
    ),
}

# if we SSH into a node, default place to look for logs
_EMR_LOG_DIR = '/mnt/var/log'

# Prior to AMI 2.4.8/3.1.1, there is a limit of 256 steps total per cluster.
# We issue a warning for users who are continuing to used pooling on these
# very old AMIs
_IMAGE_SUPPORTS_POOLING = {
    '2': False,
    '2.4.8': True,
    '3': False,
    '3.1.1': True,
}

_MAX_SSH_RETRIES = 20

# ssh should fail right away if it can't bind a port
_WAIT_FOR_SSH_TO_FAIL = 1.0

# amount of time to wait between checks for available pooled clusters
_POOLING_SLEEP_INTERVAL = 30.01  # Add .1 seconds so minutes arent spot on.

# bootstrap action which automatically terminates idle clusters
_MAX_MINS_IDLE_BOOTSTRAP_ACTION_PATH = os.path.join(
    os.path.dirname(mrjob.__file__),
    'bootstrap',
    'terminate_idle_cluster_emr.sh')

# default AWS region to use for EMR. Using us-west-2 because it is the default
# for new (since October 10, 2012) accounts (see #1025)
_DEFAULT_EMR_REGION = 'us-west-2'

# default AMI to use on EMR. This may be updated with each version
_DEFAULT_IMAGE_VERSION = '6.0.0'

# first AMI version that we can't run bash -e on (see #1548)
_BAD_BASH_IMAGE_VERSION = '5.2.0'

# use this if bash -e works (/bin/sh is actually bash)
_GOOD_BASH_SH_BIN = ['/bin/sh', '-ex']

# use this if bash -e doesn't work
_BAD_BASH_SH_BIN = ['/bin/sh', '-x']

# Hadoop streaming jar on 1-3.x AMIs
_PRE_4_X_STREAMING_JAR = '/home/hadoop/contrib/streaming/hadoop-streaming.jar'

# intermediary jar used on 4.x AMIs
_4_X_COMMAND_RUNNER_JAR = 'command-runner.jar'

# path to spark-submit on 3.x AMIs. (On 4.x, it's just 'spark-submit')
_3_X_SPARK_SUBMIT = '/home/hadoop/spark/bin/spark-submit'

# bootstrap action to install Spark on 3.x AMIs (On 4.x+, we use
# Applications instead)
_3_X_SPARK_BOOTSTRAP_ACTION = (
    'file:///usr/share/aws/emr/install-spark/install-spark')

# first AMI version to support Spark
_MIN_SPARK_AMI_VERSION = '3.8.0'

# first AMI version with Spark that supports Python 3
_MIN_SPARK_PY3_AMI_VERSION = '4.0.0'

# first AMI version that allows steps to run concurrently
_MIN_STEP_CONCURRENCY_AMI_VERSION = '5.28.0'

# we have to wait this many minutes for logs to transfer to S3 (or wait
# for the cluster to terminate). Docs say logs are transferred every 5
# minutes, but I've seen it take longer on the 4.3.0 AMI. Probably it's
# 5 minutes plus time to copy the logs, or something like that.
_S3_LOG_WAIT_MINUTES = 10

# minimum amount of memory to run spark jobs
#
# it's possible that we could get by with slightly less memory, but
# m1.medium (3.75) definitely doesn't work.
_MIN_SPARK_INSTANCE_MEMORY = 7.5

# these are the only kinds of instance roles that exist
_INSTANCE_ROLES = ('MASTER', 'CORE', 'TASK')

# where to find the history log in HDFS
_YARN_HDFS_HISTORY_LOG_DIR = 'hdfs:///tmp/hadoop-yarn/staging/history'

# mildly flexible regex to detect cluster self-termination. Termination of
# non-master nodes won't shut down the cluster, so don't need to match that.
_CLUSTER_SELF_TERMINATED_RE = re.compile(
    '^.*(node|instances) .* terminated.*$', re.I)

# if this appears in an S3 object's "restore" field, the object
# is available to read even if it's Glacier-archived
_RESTORED_FROM_GLACIER = 'ongoing-request="false"'

# Amount of time in seconds before we timeout yarn api calls.
_YARN_API_TIMEOUT = 20

# which port to connect to the YARN resource manager on
_YARN_RESOURCE_MANAGER_PORT = 8088

# base path for YARN resource manager
_YRM_BASE_PATH = '/ws/v1/cluster'

# all the cluster states other than terminating/terminated. We need this list
# because the ListClusters call can't filter out unwanted cluster states;
# it can only accept a whitelist of desired ones
#
# valid states are here:
# https://docs.aws.amazon.com/emr/latest/APIReference/API_ListClusters.html
_ACTIVE_CLUSTER_STATES = ['STARTING', 'BOOTSTRAPPING', 'RUNNING', 'WAITING']


# used to bail out and retry when a pooled cluster self-terminates
class _PooledClusterSelfTerminatedException(Exception):
    pass


if PY2:
    # this was introduced in Python 3.3
    TimeoutError = OSError


class PoolTimeoutException(TimeoutError):
    pass


class EMRJobRunner(HadoopInTheCloudJobRunner, LogInterpretationMixin):
    """Runs an :py:class:`~mrjob.job.MRJob` on Amazon Elastic MapReduce.
    Invoked when you run your job with ``-r emr``.

    :py:class:`EMRJobRunner` runs your job in an EMR cluster, which is
    basically a temporary Hadoop cluster. Normally, it creates a cluster
    just for your job; it's also possible to run your job in a specific
    cluster by setting *cluster_id* or to automatically choose a
    waiting cluster, creating one if none exists, by setting
    *pool_clusters*.

    Input, support, and jar files can be either local or on S3; use
    ``s3://...`` URLs to refer to files on S3.

    This class has some useful utilities for talking directly to S3 and EMR,
    so you may find it useful to instantiate it without a script::

        from mrjob.emr import EMRJobRunner

        emr_client = EMRJobRunner().make_emr_client()
        clusters = emr_client.list_clusters()
        ...
    """
    alias = 'emr'

    OPT_NAMES = HadoopInTheCloudJobRunner.OPT_NAMES | {
        'add_steps_in_batch',
        'additional_emr_info',
        'applications',
        'aws_access_key_id',
        'aws_secret_access_key',
        'aws_session_token',
        'bootstrap_actions',
        'bootstrap_spark',
        'cloud_log_dir',
        'core_instance_bid_price',
        'docker_client_config',
        'docker_image',
        'docker_mounts',
        'ebs_root_volume_gb',
        'ec2_endpoint',
        'ec2_key_pair',
        'ec2_key_pair_file',
        'emr_action_on_failure',
        'emr_configurations',
        'emr_endpoint',
        'enable_emr_debugging',
        'hadoop_extra_args',
        'iam_endpoint',
        'iam_instance_profile',
        'iam_service_role',
        'instance_fleets',
        'instance_groups',
        'master_instance_bid_price',
        'max_clusters_in_pool',
        'max_concurrent_steps',
        'min_available_mb',
        'min_available_virtual_cores',
        'pool_clusters',
        'pool_jitter_seconds',
        'pool_name',
        'pool_timeout_minutes',
        'pool_wait_minutes',
        'release_label',
        's3_endpoint',
        'ssh_add_bin',
        'ssh_bin',
        'ssh_bind_ports',
        'ssh_tunnel',
        'ssh_tunnel_is_open',
        'subnet',
        'tags',
        'task_instance_bid_price',
    }

    # supports everything (so far)
    _STEP_TYPES = {
        'jar', 'spark', 'spark_jar', 'spark_script', 'streaming'}

    # everything that controls instances number, type, or price
    _INSTANCE_OPT_NAMES = {
        name for name in OPT_NAMES
        if 'instance' in name and 'iam' not in name
    }

    def __init__(self, **kwargs):
        """:py:class:`~mrjob.emr.EMRJobRunner` takes the same arguments as
        :py:class:`~mrjob.runner.MRJobRunner`, plus some additional options
        which can be defaulted in :ref:`mrjob.conf <mrjob.conf>`.

        *aws_access_key_id* and *aws_secret_access_key* are required if you
        haven't set them up already for boto3 (e.g. by setting the environment
        variables :envvar:`AWS_ACCESS_KEY_ID` and
        :envvar:`AWS_SECRET_ACCESS_KEY`)

        A lengthy list of additional options can be found in
        :doc:`guides/emr-opts.rst`.
        """
        super(EMRJobRunner, self).__init__(**kwargs)

        self._fix_s3_tmp_and_log_uri_opts()

        # use job key to make a unique tmp dir
        self._cloud_tmp_dir = self._opts['cloud_tmp_dir'] + self._job_key + '/'

        # pick/validate output dir
        if self._output_dir:
            self._output_dir = self._check_and_fix_s3_dir(self._output_dir)
        else:
            self._output_dir = self._cloud_tmp_dir + 'output/'

        # check AMI version
        if self._opts['image_version'].startswith('1.'):
            log.warning('1.x AMIs will not work because they use'
                        ' Python 2.5. Use a later AMI version or mrjob v0.4.2')
        elif not version_gte(self._opts['image_version'], '2.4.3'):
            log.warning("AMIs prior to 2.4.3 probably will not work because"
                        " they don't support Python 2.7.")
        elif not self._image_version_gte('5.7.0'):
            if self._opts['image_id']:
                log.warning('AMIs prior to 5.7.0 will probably not work'
                            ' with custom machine images')

        if self._opts['pool_clusters'] and not map_version(
                self._opts['image_version'], _IMAGE_SUPPORTS_POOLING):
            log.warning(
                "Cluster pooling is not fully supported on AMIs prior to"
                " 2.4.8/3.1.1 due to the limit on total number of steps")

        if self._opts['max_concurrent_steps'] < 1:
            raise ValueError('max_concurrent_steps must be at least 1')

        # manage local files that we want to upload to S3. We'll add them
        # to this manager just before we need them.
        s3_files_dir = self._cloud_tmp_dir + 'files/'
        self._upload_mgr = UploadDirManager(s3_files_dir)

        # master node setup script (handled later by
        # _add_master_node_setup_files_for_upload())
        self._master_node_setup_mgr = WorkingDirManager()
        self._master_node_setup_script_path = None

        # where our own logs ended up (we'll find this out once we run the job)
        self._s3_log_dir_uri = None

        # did we create the cluster we're running on?
        self._created_cluster = False

        # did we acquire a lock on self._cluster_id? (used for pooling)
        self._locked_cluster = None

        # IDs of steps we have submitted to the cluster
        self._step_ids = []

        # we don't upload the ssh key to master until it's needed
        self._ssh_key_is_copied = False

        # map from cluster ID to a dictionary containing cached info about
        # that cluster. Includes the following keys:
        #
        # - image_version
        # - hadoop_version
        # - master_public_dns
        # - master_private_ip
        #
        # (we may do this for multiple cluster IDs if we join a pooled cluster
        # that self-terminates)
        self._cluster_to_cache = defaultdict(dict)

        # set of cluster IDs for which we logged the master node's public DNS
        self._logged_address_of_master = set()

        # List of dicts (one for each step) potentially containing
        # the keys 'history', 'step', and 'task'. These will also always
        # contain 'step_id' (the s-XXXXXXXX step ID on EMR).
        #
        # This will be filled by _wait_for_steps_to_complete()
        #
        # This might work better as a dictionary.
        self._log_interpretations = []

        # log interpretation for master node setup step (currently we don't
        # use this for anything; we just want to keep it out of
        # self._log_interpretations)
        self._mns_log_interpretation = None

        # set of step numbers (0-indexed) where we waited 5 minutes for logs to
        # transfer to S3 (so we don't do it twice)
        self._waited_for_logs_on_s3 = set()

        # info used to match clusters. catches _pool_hash_dict()
        self._pool_hash_dict_cached = None

        # add_steps_in_batch and concurrent steps don't mix
        if (self._add_steps_in_batch() and
                self._opts['max_concurrent_steps'] > 1):
            log.warning('add_steps_in_batch will probably not work'
                        ' with max_concurrent_steps > 1')

        # min_available_* options require SSH
        if ((self._opts['min_available_mb'] or
                self._opts['min_available_virtual_cores']) and
                not (self._opts['ec2_key_pair'] and
                     self._opts['ec2_key_pair_file'])):
            raise ValueError('you must set up SSH (ec2_key_pair and'
                             ' ec2_key_pair_file) to use the'
                             ' min_available_* options')

    ### Options ###

    @classmethod
    def _default_opts(cls):
        return combine_dicts(
            super(EMRJobRunner, cls)._default_opts(),
            dict(
                bootstrap_python=None,
                check_cluster_every=30,
                cleanup_on_failure=['JOB'],
                cloud_fs_sync_secs=5.0,
                docker_client_config=None,
                docker_image=None,
                image_version=_DEFAULT_IMAGE_VERSION,
                max_concurrent_steps=1,
                min_available_mb=0,
                min_available_virtual_cores=0,
                num_core_instances=0,
                num_task_instances=0,
                pool_clusters=False,
                pool_name='default',
                pool_jitter_seconds=60,
                pool_wait_minutes=0,
                region=_DEFAULT_EMR_REGION,
            )
        )

    def _combine_opts(self, opt_list):
        """Blank out overriden *instance_fleets* and *instance_groups*

        Convert image_version of 4.x and later to release_label."""
        # blank out any instance_fleets/groups before the last config
        # where they are set
        opt_list = _blank_out_conflicting_opts(
            opt_list,
            ['instance_fleets', 'instance_groups'],
            self._INSTANCE_OPT_NAMES)

        # now combine opts, with instance_groups/fleets blanked out
        opts = super(EMRJobRunner, self)._combine_opts(opt_list)

        # set release_label based on image_version
        if (version_gte(opts['image_version'], '4') and
                not opts['release_label']):
            opts['release_label'] = 'emr-' + opts['image_version']

        # don't keep two confs with the same Classification (see #2097)
        opts['emr_configurations'] = _deduplicate_emr_configurations(
            opts['emr_configurations'])

        return opts

    def _fix_opt(self, opt_key, opt_value, source):
        """Fix and check various EMR-specific options"""
        opt_value = super(EMRJobRunner, self)._fix_opt(
            opt_key, opt_value, source)

        # *_instance_bid_price
        if opt_key.endswith('_instance_bid_price'):
            if not opt_value:  # don't allow blank bid price
                return None

            try:
                if not float(opt_value):
                    return None
            except ValueError:  # maybe EMR allows non-floats?
                pass

            return str(opt_value)  # should be str, not a number

        # additional_emr_info
        elif opt_key == 'additional_emr_info' and not isinstance(
                opt_value, string_types):
            return json.dumps(opt_value)

        # emr_configurations
        elif opt_key == 'emr_configurations':
            return [_fix_configuration_opt(c) for c in opt_value]

        # region
        elif opt_key == 'region':
            # don't allow blank region
            return opt_value or _DEFAULT_EMR_REGION

        # subnet should be None, a string, or a multi-item list
        elif opt_key == 'subnet':
            return _fix_subnet_opt(opt_value)

        else:
            return opt_value

    def _obfuscate_opt(self, opt_key, opt_value):
        """Obfuscate AWS credentials."""
        # don't need to obfuscate empty values
        if not opt_value:
            return opt_value

        if opt_key in ('aws_secret_access_key', 'aws_session_token'):
            # don't expose any part of secret credentials
            return '...'
        elif opt_key == 'aws_access_key_id':
            if isinstance(opt_value, string_types):
                return '...' + opt_value[-4:]
            else:
                # don't expose aws_access_key_id if it was accidentally
                # put in a list or something
                return '...'
        else:
            return opt_value

    def _image_version_gte(self, version):
        """Check if the requested image version is greater than
        or equal to *version*. If the *release_label* opt is set,
        look at that instead.

        If you're checking the actual image version of a cluster, just
        use :py:func:`~mrjob.compat.version_gte` and
        :py:meth:`get_image_version`.
        """
        if self._opts['release_label']:
            return version_gte(
                self._opts['release_label'].lstrip('emr-'), version)
        else:
            return version_gte(self._opts['image_version'], version)

    def _fix_s3_tmp_and_log_uri_opts(self):
        """Fill in cloud_tmp_dir and cloud_log_dir (in self._opts) if they
        aren't already set.

        Helper for __init__.
        """
        # set cloud_tmp_dir by checking for existing buckets
        if not self._opts['cloud_tmp_dir']:
            self._set_cloud_tmp_dir()
            log.info('Using %s as our temp dir on S3' %
                     self._opts['cloud_tmp_dir'])

        self._opts['cloud_tmp_dir'] = self._check_and_fix_s3_dir(
            self._opts['cloud_tmp_dir'])

        # set cloud_log_dir
        if self._opts['cloud_log_dir']:
            self._opts['cloud_log_dir'] = self._check_and_fix_s3_dir(
                self._opts['cloud_log_dir'])
        else:
            self._opts['cloud_log_dir'] = self._opts['cloud_tmp_dir'] + 'logs/'

    def _set_cloud_tmp_dir(self):
        """Helper for _fix_s3_tmp_and_log_uri_opts"""
        client = self.fs.s3.make_s3_client()

        for bucket_name in self.fs.s3.get_all_bucket_names():
            if not bucket_name.startswith('mrjob-'):
                continue

            bucket_region = _get_bucket_region(client, bucket_name)
            if bucket_region == self._opts['region']:
                # Regions are both specified and match
                log.debug("using existing temp bucket %s" % bucket_name)
                self._opts['cloud_tmp_dir'] = 's3://%s/tmp/' % bucket_name
                return

        # That may have all failed. If so, pick a name.
        bucket_name = 'mrjob-' + random_identifier()
        self._opts['cloud_tmp_dir'] = 's3://%s/tmp/' % bucket_name
        log.info('Auto-created temp S3 bucket %s' % bucket_name)
        self._wait_for_s3_eventual_consistency()

    def _s3_log_dir(self):
        """Get the URI of the log directory for this job's cluster."""
        if not self._s3_log_dir_uri:
            cluster = self._describe_cluster()
            log_uri = cluster.get('LogUri')
            if log_uri:
                self._s3_log_dir_uri = '%s%s/' % (
                    log_uri.replace('s3n://', 's3://'), self._cluster_id)

        return self._s3_log_dir_uri

    def _check_and_fix_s3_dir(self, s3_uri):
        """Helper for __init__"""
        if not is_s3_uri(s3_uri):
            raise ValueError('Invalid S3 URI: %r' % s3_uri)
        if not s3_uri.endswith('/'):
            s3_uri = s3_uri + '/'

        return s3_uri

    def _bash_is_bad(self):
        # hopefully, there will eventually be an image version
        # where this issue is fixed. See #1548
        return self._image_version_gte(_BAD_BASH_IMAGE_VERSION)

    def _default_sh_bin(self):
        if self._bash_is_bad():
            return _BAD_BASH_SH_BIN
        else:
            return _GOOD_BASH_SH_BIN

    def _sh_pre_commands(self):
        if self._bash_is_bad() and not self._opts['sh_bin']:
            return ['set -e']
        else:
            return []

    @property
    def fs(self):
        """:py:class:`~mrjob.fs.base.Filesystem` object for SSH, S3, and the
        local filesystem.
        """
        if self._fs is None:
            self._fs = CompositeFilesystem()

            if self._opts['ec2_key_pair_file']:
                self._fs.add_fs('ssh', SSHFilesystem(
                    ssh_bin=self._ssh_bin(),
                    ssh_add_bin=self._ssh_add_bin(),
                    ec2_key_pair_file=self._opts['ec2_key_pair_file']))

            self._fs.add_fs('s3', S3Filesystem(
                aws_access_key_id=self._opts['aws_access_key_id'],
                aws_secret_access_key=self._opts['aws_secret_access_key'],
                aws_session_token=self._opts['aws_session_token'],
                s3_endpoint=self._opts['s3_endpoint'],
                s3_region=self._opts['region'],
                part_size=self._upload_part_size()))

            if self._opts['ec2_key_pair_file']:
                # add hadoop fs after S3 because it tries to handle all URIs

                # we'll set hadoop_bin later, once the cluster is set up
                self._fs.add_fs('hadoop', HadoopFilesystem(hadoop_bin=[]))

            self._fs.add_fs('local', LocalFilesystem())

        return self._fs

    def _run(self):
        self._launch()
        self._finish_run()

    def _finish_run(self):
        while True:
            try:
                self._wait_for_steps_to_complete()
                break
            except _PooledClusterSelfTerminatedException:
                self._relaunch()

    def _prepare_for_launch(self):
        """Set up files needed for the job."""
        self._check_output_not_exists()
        self._create_setup_wrapper_scripts()
        self._add_bootstrap_files_for_upload()
        self._add_master_node_setup_files_for_upload()
        self._add_job_files_for_upload()
        self._upload_local_files()
        # make sure we can see the files we copied to S3
        self._wait_for_s3_eventual_consistency()

    def _launch(self):
        """Set up files and then launch our job on EMR."""
        self._prepare_for_launch()
        self._launch_emr_job()

    def _relaunch(self):
        # files are already in place; just start with a fresh cluster
        assert not self._opts['cluster_id']
        self._cluster_id = None
        self._created_cluster = False
        self._step_ids = []

        # old SSH tunnel isn't valid for this cluster (see #1549)
        if self._ssh_proc:
            self._kill_ssh_tunnel()

        # don't try to connect to HDFS on the old cluster
        if hasattr(self.fs, 'hadoop'):
            self.fs.hadoop.set_hadoop_bin([])

        self._launch_emr_job()

    def _check_input_path(self, path):
        """Add a custom check for S3 paths to ensure they're not in
        Glacier (which causes a cryptic error). See #1887."""
        # handle non-S3 paths the usual way
        if not is_s3_uri(path):
            super(EMRJobRunner, self)._check_input_path(path)
            return

        exists = False

        for uri, obj in self.fs.s3._ls(path):
            exists = True

            # we currently just look for 'ongoing-request="false"'
            # in the *restore* field and ignore the expiration date
            # (if the object has expired, the *restore* field won't be set).
            #
            # See #1887 for more discussion of checking expiration.
            if obj.storage_class == 'GLACIER' and not (
                    obj.restore and _RESTORED_FROM_GLACIER in obj.restore):
                raise IOError(
                    '%s is archived in Glacier and'
                    ' cannot be read as input!' % uri)

        if not exists:
            raise IOError(
                'Input path %s does not exist!' % (path,))

    def _check_output_not_exists(self):
        """Verify the output path does not already exist. This avoids
        provisioning a cluster only to have Hadoop refuse to launch.
        """
        try:
            if self.fs.exists(self._output_dir):
                raise IOError(
                    'Output path %s already exists!' % (self._output_dir,))
        except botocore.exceptions.ClientError:
            pass

    def _add_bootstrap_files_for_upload(self, persistent=False):
        """Add files needed by the bootstrap script to self._upload_mgr.

        Create the master bootstrap script if necessary.

        persistent -- set by make_persistent_cluster()
        """
        # all other files needed by the script are already in
        # _bootstrap_dir_mgr
        for path in self._bootstrap_dir_mgr.paths():
            self._upload_mgr.add(path)

        # now that we know where the above files live, we can create
        # the master bootstrap script
        self._create_master_bootstrap_script_if_needed()
        if self._master_bootstrap_script_path:
            self._upload_mgr.add(self._master_bootstrap_script_path)

        # make sure bootstrap action scripts are on S3
        for bootstrap_action in self._bootstrap_actions():
            self._upload_mgr.add(bootstrap_action['path'])

        # Add max-mins-idle script if we need it
        if persistent or self._opts['pool_clusters']:
            self._upload_mgr.add(_MAX_MINS_IDLE_BOOTSTRAP_ACTION_PATH)

    def _add_master_node_setup_files_for_upload(self):
        """Add files necesary for the master node setup script to
        self._master_node_setup_mgr() and self._upload_mgr().

        Create the master node setup script if necessary.
        """
        # currently, only used by libjars; see #1336 for how we might open
        # this up more generally
        for path in self._opts['libjars']:
            # passthrough for libjars already on EMR
            if path.startswith('file:///'):
                continue

            self._master_node_setup_mgr.add('file', path)
            self._upload_mgr.add(path)

        self._create_master_node_setup_script_if_needed()
        if self._master_node_setup_script_path:
            self._upload_mgr.add(self._master_node_setup_script_path)

    def _add_job_files_for_upload(self):
        """Add files needed for running the job (setup and input)
        to self._upload_mgr."""
        for path in self._py_files():
            self._upload_mgr.add(path)

        if self._opts['hadoop_streaming_jar']:
            self._upload_mgr.add(self._opts['hadoop_streaming_jar'])

        # upload JARs and (Python) scripts run by steps
        for step in self._get_steps():
            for key in 'jar', 'script':
                if step.get(key):
                    self._upload_mgr.add(step[key])

    def _ssh_add_bin(self):
        # the args of the ssh-add binary
        return self._opts['ssh_add_bin'] or ['ssh-add']

    def _ssh_bin(self):
        # the args of the ssh binary
        return self._opts['ssh_bin'] or ['ssh']

    def _set_up_ssh_tunnel_and_hdfs(self):
        if hasattr(self.fs, 'hadoop'):
            self.fs.hadoop.set_hadoop_bin(self._ssh_hadoop_bin())
        self._set_up_ssh_tunnel()

    def _ssh_tunnel_config(self):
        """Look up AMI version, and return a dict with the following keys:

        name: "job tracker" or "resource manager"
        path: path to start page of job tracker/resource manager
        port: port job tracker/resource manager is running on.
        """
        return map_version(self.get_image_version(),
                           _IMAGE_VERSION_TO_SSH_TUNNEL_CONFIG)

    def _job_tracker_host(self):
        """The host of the job tracker/resource manager, from the master node.
        """
        tunnel_config = self._ssh_tunnel_config()

        if tunnel_config['localhost']:
            # Issue #1311: on the 2.x AMIs, we want to tunnel to the job
            # tracker on localhost; otherwise it won't
            # work on some VPC setups.
            return 'localhost'
        else:
            # Issue #1397: on the 3.x and 4.x AMIs we want to tunnel to the
            # resource manager on the master node's *internal* IP; otherwise
            # it work won't work on some VPC setups
            return self._master_private_ip()

    def _ssh_tunnel_args(self, bind_port):
        for opt_name in ('ec2_key_pair', 'ec2_key_pair_file',
                         'ssh_bind_ports'):
            if not self._opts[opt_name]:
                log.warning(
                    "  You must set %s in order to set up the SSH tunnel!"
                    % opt_name)
                self._give_up_on_ssh_tunnel = True
                return

        host = self._address_of_master()
        if not host:
            return

        return self._ssh_bin() + [
            '-o', 'VerifyHostKeyDNS=no',
            '-o', 'StrictHostKeyChecking=no',
            '-o', 'ExitOnForwardFailure=yes',
            '-o', 'UserKnownHostsFile=%s' % os.devnull,
        ] + self._ssh_tunnel_opts(bind_port) + [
            '-i', self._opts['ec2_key_pair_file'],
            'hadoop@%s' % host,
        ]

    def _ssh_hadoop_bin(self):
        if not self._opts['ec2_key_pair_file']:
            return []

        host = self._address_of_master()
        if not host:
            return []

        return self._ssh_bin() + [
            '-o', 'VerifyHostKeyDNS=no',
            '-o', 'StrictHostKeyChecking=no',
            '-o', 'ExitOnForwardFailure=yes',
            '-o', 'UserKnownHostsFile=%s' % os.devnull,
            '-i', self._opts['ec2_key_pair_file'],
            '-q',  # don't care about SSH warnings, we just want hadoop
            'hadoop@%s' % host,
            'hadoop',
        ]

    def _job_tracker_url(self):
        """Not actually used to set up the SSH tunnel, used to run curl
        over SSH to fetch from the job tracker directly."""
        tunnel_config = self._ssh_tunnel_config()

        return 'http://%s:%d%s' % (
            self._job_tracker_host(),
            tunnel_config['port'],
            tunnel_config['path'])

    ### Running the job ###

    def cleanup(self, mode=None):
        super(EMRJobRunner, self).cleanup(mode=mode)

        # always stop our SSH tunnel if it's still running
        self._kill_ssh_tunnel()

        # stop the cluster if it belongs to us (it may have stopped on its
        # own already, but that's fine)
        # don't stop it if it was created due to --pool because the user
        # probably wants to use it again
        if self._cluster_id and not self._opts['cluster_id'] \
                and not self._opts['pool_clusters']:
            log.info('Terminating cluster: %s' % self._cluster_id)
            try:
                self.make_emr_client().terminate_job_flows(
                    JobFlowIds=[self._cluster_id]
                )
            except Exception as e:
                log.exception(e)

        # TODO: otherwise, cancel any steps we submitted (#1570)

    def _cleanup_cloud_tmp(self):
        # delete all the files we created on S3
        if self._cloud_tmp_dir:
            try:
                log.info('Removing s3 temp directory %s...' %
                         self._cloud_tmp_dir)
                self.fs.rm(self._cloud_tmp_dir)
                self._cloud_tmp_dir = None
            except Exception as e:
                log.exception(e)

    def _cleanup_logs(self):
        super(EMRJobRunner, self)._cleanup_logs()

        # delete the log files, if it's a cluster we created (the logs
        # belong to the cluster)
        if self._s3_log_dir() and not self._opts['cluster_id'] \
                and not self._opts['pool_clusters']:
            try:
                log.info('Removing log files in %s...' % self._s3_log_dir())
                self.fs.rm(self._s3_log_dir())
            except Exception as e:
                log.exception(e)

    def _cleanup_cluster(self):
        if not self._cluster_id:
            # If we don't have a cluster, then we can't terminate it.
            return

        emr_client = self.make_emr_client()
        try:
            log.info("Attempting to terminate cluster")
            emr_client.terminate_job_flows(
                JobFlowIds=[self._cluster_id]
            )
        except Exception as e:
            # Something happened with boto3 and the user should know.
            log.exception(e)
            return
        log.info('Cluster %s successfully terminated' % self._cluster_id)

    def _wait_for_s3_eventual_consistency(self):
        """Sleep for a little while, to give S3 a chance to sync up.
        """
        log.debug('Waiting %.1fs for S3 eventual consistency...' %
                  self._opts['cloud_fs_sync_secs'])
        time.sleep(self._opts['cloud_fs_sync_secs'])

    def _wait_for_cluster_to_terminate(self, cluster=None):
        if not cluster:
            cluster = self._describe_cluster()

        log.info('Waiting for cluster (%s) to terminate...' %
                 cluster['Id'])

        if (cluster['Status']['State'] == 'WAITING' and
                cluster['AutoTerminate']):
            raise Exception('Operation requires cluster to terminate, but'
                            ' it may never do so.')

        while True:
            log.info('  %s' % cluster['Status']['State'])

            if cluster['Status']['State'] in (
                    'TERMINATED', 'TERMINATED_WITH_ERRORS'):
                return

            time.sleep(self._opts['check_cluster_every'])
            cluster = self._describe_cluster()

    # instance types

    def _instance_type(self, role):
        """What instance type should we use for the given role?
        (one of 'MASTER', 'CORE', 'TASK')"""
        if role not in _INSTANCE_ROLES:
            raise ValueError

        # explicitly set
        if self._opts[role.lower() + '_instance_type']:
            return self._opts[role.lower() + '_instance_type']

        elif self._instance_is_worker(role):
            # using *instance_type* here is defensive programming;
            # if set, it should have already been popped into the worker
            # instance type option(s) by _fix_instance_opts() above
            return self._opts['instance_type'] or self._default_instance_type()
        else:
            return self._default_instance_type()

    def _default_instance_type(self):
        """Default instance type if not set by the user."""
        # m5.xlarge is available on all regions, but only works in AMI 5.13.0
        # or later. See #2098.
        if self._image_version_gte('5.13.0'):
            return 'm5.xlarge'
        else:
            return 'm4.large'

    def _instance_is_worker(self, role):
        """Do instances of the given role run tasks? True for non-master
        instances and sole master instance."""
        if role not in _INSTANCE_ROLES:
            raise ValueError

        return (role != 'MASTER' or
                sum(self._num_instances(role)
                    for role in _INSTANCE_ROLES) == 1)

    def _num_instances(self, role):
        """How many of the given instance type do we want?"""
        if role not in _INSTANCE_ROLES:
            raise ValueError

        if role == 'MASTER':
            return 1  # there can be only one
        else:
            return self._opts['num_' + role.lower() + '_instances']

    def _instance_bid_price(self, role):
        """What's the bid price for the given role (if any)?"""
        if role not in _INSTANCE_ROLES:
            raise ValueError

        return self._opts[role.lower() + '_instance_bid_price']

    def _instance_groups(self):
        """Which instance groups do we want to request?

        Returns the value of the ``InstanceGroups`` parameter
        passed to the EMR API.
        """
        if self._opts['instance_groups']:
            return self._opts['instance_groups']

        return [
            _build_instance_group(
                role=role,
                instance_type=self._instance_type(role),
                num_instances=self._num_instances(role),
                bid_price=self._instance_bid_price(role),
            )
            for role in _INSTANCE_ROLES
            if self._num_instances(role)
        ]

    def _create_cluster(self, persistent=False):
        """Create an empty cluster on EMR, and return the ID of that
        job.

        If the ``tags`` option is set, also tags the cluster (which
        is a separate API call).

        persistent -- if this is true, create the cluster with the keep_alive
            option, indicating the job will have to be manually terminated.
        """
        log.debug('Creating Elastic MapReduce cluster')
        emr_client = self.make_emr_client()

        kwargs = self._cluster_kwargs(persistent)
        log.debug('Calling run_job_flow(%s)' % (
            ', '.join('%s=%r' % (k, v)
                      for k, v in sorted(kwargs.items()))))
        cluster_id = emr_client.run_job_flow(**kwargs)['JobFlowId']

        log.info('Created new cluster %s' % cluster_id)

        # set EMR tags for the cluster
        tags = dict(self._opts['tags'])

        # patch in version
        tags['__mrjob_version'] = mrjob.__version__

        # patch in cluster label and owner
        tags['__mrjob_label'] = self._label()
        tags['__mrjob_owner'] = self._owner()

        # add pooling tags
        if self._opts['pool_clusters']:
            tags['__mrjob_pool_hash'] = self._pool_hash()
            tags['__mrjob_pool_name'] = self._opts['pool_name']

        self._add_tags(tags, cluster_id)

        return cluster_id

    def _add_tags(self, tags, cluster_id):
        """Add tags in the dict *tags* to cluster *cluster_id*. Do nothing
        if *tags* is empty or ``None``"""
        if not tags:
            return

        tags_items = sorted(tags.items())

        self.make_emr_client().add_tags(
            ResourceId=cluster_id,
            Tags=[dict(Key=k, Value=v) for k, v in tags_items])

        log.info('Added EMR tags to cluster %s: %s' % (
            cluster_id,
            ', '.join('%s=%s' % (tag, value) for tag, value in tags_items)))

    # TODO: could break this into sub-methods for clarity
    def _cluster_kwargs(self, persistent=False):
        """Build kwargs for emr_client.run_job_flow()"""
        kwargs = {}

        kwargs['Name'] = self._job_key + self._cluster_name_pooling_suffix()

        kwargs['LogUri'] = self._opts['cloud_log_dir']

        if self._opts['release_label']:
            kwargs['ReleaseLabel'] = self._opts['release_label']
        else:
            kwargs['AmiVersion'] = self._opts['image_version']

        if self._opts['image_id']:
            kwargs['CustomAmiId'] = self._opts['image_id']

        # capitalizing Instances because it's just an API parameter
        kwargs['Instances'] = Instances = {}

        if self._opts['zone']:
            Instances['Placement'] = dict(AvailabilityZone=self._opts['zone'])

        if self._opts['instance_fleets']:
            Instances['InstanceFleets'] = self._opts['instance_fleets']
        else:
            Instances['InstanceGroups'] = self._instance_groups()

        # EBS Root volume size
        if self._opts['ebs_root_volume_gb']:
            kwargs['EbsRootVolumeSize'] = self._opts['ebs_root_volume_gb']

        # bootstrap actions
        kwargs['BootstrapActions'] = BootstrapActions = []

        for i, bootstrap_action in enumerate(self._bootstrap_actions()):
            uri = self._upload_mgr.uri(bootstrap_action['path'])
            BootstrapActions.append(dict(
                Name=('action %d' % i),
                ScriptBootstrapAction=dict(
                    Path=uri,
                    Args=bootstrap_action['args'])))

        if self._master_bootstrap_script_path:
            uri = self._upload_mgr.uri(self._master_bootstrap_script_path)

            BootstrapActions.append(dict(
                Name='master',
                ScriptBootstrapAction=dict(
                    Path=uri,
                    Args=[])))

        if persistent or self._opts['pool_clusters']:
            Instances['KeepJobFlowAliveWhenNoSteps'] = True

            # use idle termination script on persistent clusters
            # add it last, so that we don't count bootstrapping as idle time
            uri = self._upload_mgr.uri(
                _MAX_MINS_IDLE_BOOTSTRAP_ACTION_PATH)

            # script takes args in (integer) seconds
            ba_args = [str(int(self._opts['max_mins_idle'] * 60))]
            BootstrapActions.append(dict(
                Name='idle timeout',
                ScriptBootstrapAction=dict(
                    Path=uri,
                    Args=ba_args)))

        if self._opts['ec2_key_pair']:
            Instances['Ec2KeyName'] = self._opts['ec2_key_pair']

        kwargs['Steps'] = Steps = []

        kwargs['StepConcurrencyLevel'] = self._opts['max_concurrent_steps']

        if self._opts['enable_emr_debugging']:
            # other steps are added separately
            Steps.append(self._build_debugging_step())

        if self._opts['additional_emr_info']:
            kwargs['AdditionalInfo'] = self._opts['additional_emr_info']

        kwargs['VisibleToAllUsers'] = True

        kwargs['JobFlowRole'] = self._instance_profile()
        kwargs['ServiceRole'] = self._service_role()

        applications = self._applications()
        if applications:
            kwargs['Applications'] = [
                dict(Name=a) for a in sorted(applications)]

        emr_configurations = self._emr_configurations()
        if emr_configurations:
            kwargs['Configurations'] = emr_configurations

        if self._opts['subnet']:
            # handle lists of subnets (for instance fleets)
            if isinstance(self._opts['subnet'], list):
                Instances['Ec2SubnetIds'] = self._opts['subnet']
            else:
                Instances['Ec2SubnetId'] = self._opts['subnet']

        return self._add_extra_cluster_params(kwargs)

    def _instance_profile(self):
        try:
            return (self._opts['iam_instance_profile'] or
                    get_or_create_mrjob_instance_profile(
                        self.make_iam_client()))
        except botocore.exceptions.ClientError as ex:
            if _client_error_status(ex) != 403:
                raise
            log.warning(
                "Can't access IAM API, trying default instance profile: %s" %
                _FALLBACK_INSTANCE_PROFILE)
            return _FALLBACK_INSTANCE_PROFILE

    def _service_role(self):
        try:
            return (self._opts['iam_service_role'] or
                    get_or_create_mrjob_service_role(self.make_iam_client()))
        except botocore.exceptions.ClientError as ex:
            if _client_error_status(ex) != 403:
                raise
            log.warning(
                "Can't access IAM API, trying default service role: %s" %
                _FALLBACK_SERVICE_ROLE)
            return _FALLBACK_SERVICE_ROLE

    def _action_on_failure(self):
        # don't terminate other people's clusters
        if (self._opts['emr_action_on_failure']):
            return self._opts['emr_action_on_failure']
        elif not self._add_steps_in_batch():
            # concurrent clusters don't allow CANCEL_ON_WAIT
            return 'CONTINUE'
        elif (self._opts['cluster_id'] or
                self._opts['pool_clusters']):
            return 'CANCEL_AND_WAIT'
        else:
            return 'TERMINATE_CLUSTER'

    def _add_steps_in_batch(self):
        if self._opts['add_steps_in_batch'] is None:
            # by default, add steps in batch only when concurrent steps
            # are not possible
            return not self._image_version_gte('5.28.0')
        else:
            return self._opts['add_steps_in_batch']

    def _steps_to_submit(self):
        """Return a step data structures to pass to ``boto3``"""
        # quick, add the other steps before the job spins up and
        # then shuts itself down! (in practice that won't happen
        # for several minutes)
        steps = []

        if self._master_node_setup_script_path:
            steps.append(self._build_master_node_setup_step())

        for n in range(self._num_steps()):
            steps.append(self._build_step(n))

        return steps

    def _build_step(self, step_num):
        step = self._get_step(step_num)

        if step['type'] == 'streaming':
            method = self._streaming_step_hadoop_jar_step
        elif step['type'] == 'jar':
            method = self._jar_step_hadoop_jar_step
        elif _is_spark_step_type(step['type']):
            method = self._spark_step_hadoop_jar_step
        else:
            raise ValueError('Bad step type: %r' % (step['type'],))

        hadoop_jar_step = method(step_num)

        return dict(
            ActionOnFailure=self._action_on_failure(),
            HadoopJarStep=hadoop_jar_step,
            Name=self._step_name(step_num),
        )

    def _streaming_step_hadoop_jar_step(self, step_num):
        jar, step_arg_prefix = self._get_streaming_jar_and_step_arg_prefix()

        args = (step_arg_prefix +
                self._hadoop_streaming_jar_args(step_num))

        return dict(Jar=jar, Args=args)

    def _jar_step_hadoop_jar_step(self, step_num):
        step = self._get_step(step_num)

        jar = self._upload_uri_or_remote_path(step['jar'])

        args = (
            self._interpolate_jar_step_args(step['args'], step_num))

        hadoop_jar_step = dict(Jar=jar, Args=args)

        if step.get('main_class'):
            hadoop_jar_step['MainClass'] = step['main_class']

        return hadoop_jar_step

    def _spark_step_hadoop_jar_step(self, step_num):
        return dict(
            Jar=self._spark_jar(),
            Args=self._args_for_spark_step(step_num))

    def _interpolate_spark_script_path(self, path):
        if path in self._working_dir_mgr.paths():
            return self._dest_in_wd_mirror(
                path, self._working_dir_mgr.name('file', path)) or path
        else:
            return self._upload_mgr.uri(path)

    def _find_spark_submit_bin(self):
        if version_gte(self.get_image_version(), '4'):
            return ['spark-submit']
        else:
            return [_3_X_SPARK_SUBMIT]

    def _spark_master(self):
        # hard-coded for EMR
        return 'yarn'

    def _spark_deploy_mode(self):
        # hard-coded for EMR; otherwise it can't access S3
        return 'cluster'

    def _spark_jar(self):
        if version_gte(self.get_image_version(), '4'):
            return _4_X_COMMAND_RUNNER_JAR
        else:
            return self._script_runner_jar_uri()

    def _step_name(self, step_num):
        """Return something like: ``'mr_your_job Step X of Y'``"""
        return '%s: Step %d of %d' % (
            self._job_key, step_num + 1, self._num_steps())

    def _upload_uri_or_remote_path(self, path):
        """Return where *path* will be uploaded, or, if it starts with
        ``'file:///'``, a local path."""
        if path.startswith('file:///'):
            return path[7:]  # keep leading slash
        else:
            return self._upload_mgr.uri(path)

    def _build_master_node_setup_step(self):
        name = '%s: Master node setup' % self._job_key
        jar = self._script_runner_jar_uri()
        step_args = [self._upload_mgr.uri(self._master_node_setup_script_path)]

        return dict(
            Name=name,
            ActionOnFailure=self._action_on_failure(),
            HadoopJarStep=dict(
                Jar=jar,
                Args=step_args,
            )
        )

    def _libjar_paths(self):
        results = []

        # libjars should be in the working dir of the master node setup
        # script path, unless they refer to paths directly (file:///)
        for path in self._opts['libjars']:
            if path.startswith('file:///'):
                results.append(path[7:])  # keep leading slash
            else:
                results.append(posixpath.join(
                    self._master_node_setup_working_dir(),
                    self._master_node_setup_mgr.name('file', path)))

        return results

    def _get_streaming_jar_and_step_arg_prefix(self):
        if self._opts['hadoop_streaming_jar']:
            if self._opts['hadoop_streaming_jar'].startswith('file://'):
                # special case: jar is already on EMR
                # relative paths are OK (though maybe not useful)
                return self._opts['hadoop_streaming_jar'][7:], []
            else:
                return self._upload_mgr.uri(
                    self._opts['hadoop_streaming_jar']), []
        elif version_gte(self.get_image_version(), '4'):
            # 4.x AMIs use an intermediary jar
            return _4_X_COMMAND_RUNNER_JAR, ['hadoop-streaming']
        else:
            # 2.x and 3.x AMIs just use a regular old streaming jar
            return _PRE_4_X_STREAMING_JAR, []

    def _launch_emr_job(self):
        """Create an empty cluster on EMR, and set self._cluster_id to
        its ID.
        """
        # step concurrency level of a cluster we added steps to, used
        # for locking
        step_concurrency_level = None

        # try to find a cluster from the pool. basically auto-fill
        # 'cluster_id' if possible and then follow normal behavior.
        if (self._opts['pool_clusters'] and not self._cluster_id):
            cluster_id, step_concurrency_level = self._find_cluster()
            if cluster_id:
                self._cluster_id = cluster_id
                self._locked_cluster = True

        # create a cluster if we're not already using an existing one
        if not self._cluster_id:
            self._cluster_id = self._create_cluster()
            self._created_cluster = True
        else:
            log.info('Adding our job to existing cluster %s' %
                     self._cluster_id)
            self._log_address_of_master_once()

        # now that we know which cluster it is, check for Spark support
        if self._has_spark_steps():
            self._check_cluster_spark_support()

        # define our steps
        steps = self._steps_to_submit()

        if self._add_steps_in_batch():
            self._add_steps_to_cluster(steps)
        else:
            # later steps will be added one at a time
            self._add_steps_to_cluster(steps[:1])

        # if we locked a cluster with concurrent steps, we can release
        # the lock immediately
        if step_concurrency_level and step_concurrency_level > 1:
            self._release_cluster_lock()

        # learn about how fast the cluster state switches
        cluster = self._describe_cluster()
        log.debug('Cluster has state %s' % cluster['Status']['State'])

        # SSH FS uses sudo if we're on AMI 4.3.0+ (see #1244)
        if hasattr(self.fs, 'ssh') and version_gte(
                self.get_image_version(), '4.3.0'):
            self.fs.ssh.use_sudo_over_ssh()

    def _release_cluster_lock(self):
        if not self._locked_cluster:
            return

        emr_client = self.make_emr_client()

        log.info('  releasing cluster lock')
        # this can fail, but usually it's because the cluster
        # started terminating, so only try releasing the lock once
        _attempt_to_unlock_cluster(emr_client, self._cluster_id)
        self._locked_cluster = False

    def _add_steps_to_cluster(self, steps):
        """Add steps (from _steps_to_submit()) to our cluster and append their
         IDs to self._step_ids"""
        emr_client = self.make_emr_client()

        steps_kwargs = dict(JobFlowId=self._cluster_id, Steps=steps)
        log.debug('Calling add_job_flow_steps(%s)' % ','.join(
            ('%s=%r' % (k, v)) for k, v in steps_kwargs.items()))
        step_ids = emr_client.add_job_flow_steps(**steps_kwargs)['StepIds']
        self._step_ids.extend(step_ids)

    def get_job_steps(self):
        """Fetch the steps submitted by this runner from the EMR API.

        .. deprecated:: 0.7.4

        .. versionadded:: 0.6.1
        """
        log.warning(
            'get_job_steps() is deprecated and will be removed in v0.8.0')

        return _get_job_steps(
            self.make_emr_client(), self.get_cluster_id(), self.get_job_key())

    def _wait_for_steps_to_complete(self):
        """Wait for every step of the job to complete, one by one."""
        steps = self._steps_to_submit()

        # clear out log interpretations if they were filled somehow
        self._log_interpretations = []
        self._mns_log_interpretation = None

        # open SSH tunnel if cluster is already ready
        # (this happens with pooling). See #1115
        cluster = self._describe_cluster()
        if cluster['Status']['State'] in ('RUNNING', 'WAITING'):
            self._set_up_ssh_tunnel_and_hdfs()

        for i, step in enumerate(steps):
            # if our step isn't already submitted, submit it
            if len(self._step_ids) <= i:
                self._add_steps_to_cluster(
                    steps[len(self._step_ids):i + 1])

            step_id = self._step_ids[i]
            step_name = step['Name'].split(': ')[-1]

            # treat master node setup script is treated as step -1
            if self._master_node_setup_script_path:
                step_num = i - 1
            else:
                step_num = i

            log.info('Waiting for %s (%s) to complete...' %
                     (step_name, step_id))

            self._wait_for_step_to_complete(step_id, step_num)

    def _wait_for_step_to_complete(self, step_id, step_num=None):
        """Helper for _wait_for_step_to_complete(). Wait for
        step with the given ID to complete, and fetch counters.
        If it fails, attempt to diagnose the error, and raise an
        exception.

        :param step_id: the s-XXXXXXX step ID on EMR
        :param step_num: which step this is out of the steps
                         belonging to our job (0-indexed). Master node
                         setup script, if there is one, is step -1

        This also adds an item to self._log_interpretations or sets
        self._mns_log_interpretation
        """
        log_interpretation = dict(step_id=step_id)

        # suppress warnings about missing job ID for script-runner.jar
        if step_num == -1:
            log_interpretation['no_job'] = True
            self._mns_log_interpretation = log_interpretation
        else:
            self._log_interpretations.append(log_interpretation)

        emr_client = self.make_emr_client()

        while True:
            # don't antagonize EMR's throttling
            log.debug('Waiting %.1f seconds...' %
                      self._opts['check_cluster_every'])
            time.sleep(self._opts['check_cluster_every'])

            # log address of the master node once if we have it
            self._log_address_of_master_once()

            step = emr_client.describe_step(
                ClusterId=self._cluster_id, StepId=step_id)['Step']

            if step['Status']['State'] == 'PENDING':
                cluster = self._describe_cluster()

                reason = _get_reason(cluster)
                reason_desc = (': %s' % reason) if reason else ''

                # we can open the ssh tunnel if cluster is ready (see #1115)
                if cluster['Status']['State'] in ('RUNNING', 'WAITING'):
                    self._set_up_ssh_tunnel_and_hdfs()

                log.info('  PENDING (cluster is %s%s)' % (
                    cluster['Status']['State'], reason_desc))
                continue

            elif step['Status']['State'] == 'RUNNING':

                time_running_desc = ''

                start = step['Status']['Timeline'].get('StartDateTime')
                if start:
                    time_running_desc = ' for %s' % strip_microseconds(
                        _boto3_now() - start)

                # now is the time to tunnel, if we haven't already
                self._set_up_ssh_tunnel_and_hdfs()
                log.info('  RUNNING%s' % time_running_desc)

                # don't log progress for master node setup step, because
                # it doesn't appear in job tracker
                if step_num >= 0:
                    self._log_step_progress()

                # it's safe to clean up our lock, cluster isn't WAITING
                self._release_cluster_lock()

                continue

            # we're done, will return at the end of this
            elif step['Status']['State'] == 'COMPLETED':
                log.info('  COMPLETED')
                # will fetch counters, below, and then return
            else:
                # step has failed somehow. *reason* seems to only be set
                # when job is cancelled (e.g. 'Job terminated')
                reason = _get_reason(step)
                reason_desc = (' (%s)' % reason) if reason else ''

                log.info('  %s%s' % (
                    step['Status']['State'], reason_desc))

                # print cluster status; this might give more context
                # why step didn't succeed
                cluster = self._describe_cluster()
                reason = _get_reason(cluster)
                reason_desc = (': %s' % reason) if reason else ''
                log.info('Cluster %s %s %s%s' % (
                    cluster['Id'],
                    'was' if 'ED' in cluster['Status']['State'] else 'is',
                    cluster['Status']['State'],
                    reason_desc))

                if cluster['Status']['State'] in (
                        'TERMINATING', 'TERMINATED', 'TERMINATED_WITH_ERRORS'):
                    # was it caused by a pooled cluster self-terminating?
                    # (if so, raise _PooledClusterSelfTerminatedException)
                    self._check_for_pooled_cluster_self_termination(
                        cluster, step)
                    # was it caused by IAM roles?
                    self._check_for_missing_default_iam_roles(cluster)
                    # was it because a bootstrap action failed?
                    self._check_for_failed_bootstrap_action(cluster)

            # spark steps require different log parsing. The master node
            # setup script is a JAR step (albeit one that never produces
            # counters)
            step_type = (
                self._get_step(step_num)['type'] if step_num >= 0 else 'jar')

            # step is done (either COMPLETED, FAILED, INTERRUPTED). so
            # try to fetch counters. (Except for master node setup
            # and Spark, which has no counters.)
            if step['Status']['State'] != 'CANCELLED' and step_num >= 0:
                self._log_counters(log_interpretation, step_num)

            if step['Status']['State'] == 'COMPLETED':
                return

            if step['Status']['State'] == 'FAILED':
                error = self._pick_error(log_interpretation, step_type)
                if error:
                    _log_probable_cause_of_failure(log, error)

            raise StepFailedException(
                step_num=step_num, num_steps=self._num_steps(),
                # "Step 0 of ... failed" looks weird
                step_desc=(
                    'Master node setup step' if step_num == -1 else None))

    def _log_address_of_master_once(self):
        """Log the master node's public DNS, if we haven't already"""
        # Some users like to SSH in manually. See #2007
        if not self._cluster_id:
            return

        if self._cluster_id in self._logged_address_of_master:
            return

        master_dns = self._address_of_master()

        if not master_dns:
            return

        log.info('  master node is %s' % master_dns)
        self._logged_address_of_master.add(self._cluster_id)

    def _log_step_progress(self):
        """Tunnel to the job tracker/resource manager and log the
        progress of the current step.

        (This takes no arguments; we just assume the most recent running
        job is ours, which should be correct for EMR.)
        """
        progress_html = (self._progress_html_from_tunnel() or
                         self._progress_html_over_ssh())
        if not progress_html:
            return

        tunnel_config = self._ssh_tunnel_config()

        if tunnel_config['name'] == 'job tracker':
            map_progress, reduce_progress = (
                _parse_progress_from_job_tracker(progress_html))
            if map_progress is not None:
                log.info('   map %3d%% reduce %3d%%' % (
                    map_progress, reduce_progress))
        else:
            progress = _parse_progress_from_resource_manager(
                progress_html)
            if progress is not None:
                log.info('   %5.1f%% complete' % progress)

    def _progress_html_from_tunnel(self):
        """Fetch progress by calling :py:func:`urlopen` on our ssh tunnel, or
        return ``None``."""
        if not self._ssh_tunnel_url:
            return None

        tunnel_config = self._ssh_tunnel_config()
        log.debug('  Fetching progress from %s at %s' % (
            tunnel_config['name'], self._ssh_tunnel_url))

        tunnel_handle = None
        try:
            tunnel_handle = urlopen(self._ssh_tunnel_url)
            return tunnel_handle.read()
        except Exception as e:
            log.debug('    failed: %s' % str(e))
            return None
        finally:
            if tunnel_handle:
                tunnel_handle.close()

    def _progress_html_over_ssh(self):
        """Fetch progress by running :command:`curl` over SSH, or return
        ``None``"""
        host = self._address_of_master()

        if not self._opts['ec2_key_pair_file']:
            return None

        if not host:
            return None

        tunnel_config = self._ssh_tunnel_config()
        remote_url = self._job_tracker_url()

        log.debug('  Fetching progress from %s over SSH' % (
            tunnel_config['name']))

        try:
            stdout, _ = self.fs.ssh._ssh_run(host, ['curl', remote_url])
            return stdout
        except Exception as e:
            log.debug('    failed: %s' % str(e))

        return None

    def _check_for_pooled_cluster_self_termination(self, cluster, step):
        """If failure could have been due to a pooled cluster self-terminating,
        raise _PooledClusterSelfTerminatedException"""
        # this check might not even be relevant
        if not self._opts['pool_clusters']:
            return

        if self._opts['cluster_id']:
            return

        # if a cluster we created self-terminated, something is wrong with
        # the way self-termination is set up (e.g. very low idle time)
        if self._created_cluster:
            return

        # don't check for max_mins_idle because it's possible to
        # join a self-terminating cluster without having max_mins_idle set
        # on this runner (pooling only cares about the master bootstrap script,
        # not other bootstrap actions)

        # our step should be CANCELLED (not failed)
        if step['Status']['State'] != 'CANCELLED':
            return

        # we *could* check if the step had a chance to start by checking if
        # step.status.timeline.startdatetime is set. This shouldn't happen in
        # practice, and if it did, we'd still be fine as long as the script
        # didn't write data to the output dir, so it's not worth the extra
        # code.

        # cluster should have stopped because master node failed
        # could also check for
        # cluster.status.statechangereason.code == 'INSTANCE_FAILURE'
        if not _CLUSTER_SELF_TERMINATED_RE.match(_get_reason(cluster)):
            return

        log.info('Pooled cluster self-terminated, trying again...')
        raise _PooledClusterSelfTerminatedException

    def _check_for_missing_default_iam_roles(self, cluster):
        """If cluster couldn't start due to missing IAM roles, tell
        user what to do."""
        if not cluster:
            cluster = self._describe_cluster()

        reason = _get_reason(cluster)
        if any(reason.endswith('/%s is invalid' % role)
               for role in (_FALLBACK_INSTANCE_PROFILE,
                            _FALLBACK_SERVICE_ROLE)):
            log.warning(
                '\n'
                'Ask your admin to create the default EMR roles'
                ' by following:\n\n'
                '    http://docs.aws.amazon.com/ElasticMapReduce/latest'
                '/DeveloperGuide/emr-iam-roles-creatingroles.html\n')

    def _default_step_output_dir(self):
        # put intermediate data in HDFS
        return 'hdfs:///tmp/mrjob/%s/step-output' % self._job_key

    ### LOG PARSING (implementation of LogInterpretationMixin) ###

    def _check_for_failed_bootstrap_action(self, cluster):
        """If our bootstrap actions failed, parse the stderr to find
        out why."""
        reason = _get_reason(cluster)
        action_num_and_node_id = _check_for_nonzero_return_code(reason)
        if not action_num_and_node_id:
            return

        if not self._read_logs():
            return

        # this doesn't really correspond to a step, so
        # don't bother storing it in self._log_interpretations
        bootstrap_interpretation = _interpret_emr_bootstrap_stderr(
            self.fs, self._ls_bootstrap_stderr_logs(**action_num_and_node_id))

        # should be 0 or 1 errors, since we're checking a single stderr file
        if bootstrap_interpretation.get('errors'):
            error = bootstrap_interpretation['errors'][0]
            _log_probable_cause_of_failure(log, error)

    def _ls_bootstrap_stderr_logs(self, action_num=None, node_id=None):
        """_ls_bootstrap_stderr_logs(), with logging for each log we parse."""
        if not self._read_logs():
            return

        for match in _ls_emr_bootstrap_stderr_logs(
                self.fs,
                self._stream_bootstrap_log_dirs(
                    action_num=action_num, node_id=node_id),
                action_num=action_num,
                node_id=node_id):
            log.info('  Parsing boostrap stderr log: %s' % match['path'])
            yield match

    def _stream_bootstrap_log_dirs(self, action_num=None, node_id=None):
        """Stream a single directory on S3 containing the relevant bootstrap
        stderr. Optionally, use *action_num* and *node_id* to narrow it down
        further.
        """
        if action_num is None or node_id is None:
            s3_dir_name = 'node'
        else:
            s3_dir_name = posixpath.join(
                'node', node_id, 'bootstrap-actions', str(action_num + 1))

        # dir_name=None means don't try to SSH in.
        #
        # TODO: If the failure is on the master node, we could just look in
        # /mnt/var/log/bootstrap-actions. However, if it's on a worker node,
        # we'd have to look up its internal IP using the ListInstances
        # API call. This *would* be a bit faster though. See #1346.
        return self._stream_log_dirs(
            'bootstrap logs',
            dir_name=None,  # don't SSH in
            s3_dir_name=s3_dir_name)

    def _stream_history_log_dirs(self, output_dir=None):
        """Yield lists of directories to look for the history log in."""

        if version_gte(self.get_image_version(), '4'):
            hdfs_dir_name = 'history'
            # on 4.0.0 (and possibly other versions before 4.3.0)
            # history logs aren't on the filesystem. See #1253
            dir_name = 'hadoop-mapreduce/history'
            s3_dir_name = 'hadoop-mapreduce/history'
        elif version_gte(self.get_image_version(), '3'):
            # on the 3.x AMIs, the history log is on HDFS only
            # (not even S3)
            hdfs_dir_name = 'history'
            dir_name = None
            s3_dir_name = None
        else:
            # 2.x AMIs don't use YARN, so no point in checking HDFS
            hdfs_dir_name = None
            dir_name = 'hadoop/history'
            s3_dir_name = 'jobs'

        return self._stream_log_dirs(
            'history log',
            hdfs_dir_name=hdfs_dir_name,
            dir_name=dir_name,
            s3_dir_name=s3_dir_name)

    def _stream_task_log_dirs(self, application_id=None, output_dir=None):
        """Get lists of directories to look for the task logs in."""
        if version_gte(self.get_image_version(), '4'):
            # denied access on some 4.x AMIs by the yarn user, see #1244
            dir_name = 'hadoop-yarn/containers'
            s3_dir_name = 'containers'
        else:
            dir_name = 'hadoop/userlogs'
            s3_dir_name = 'task-attempts'

        if application_id:
            dir_name = posixpath.join(dir_name, application_id)
            s3_dir_name = posixpath.join(s3_dir_name, application_id)

        return self._stream_log_dirs(
            'task logs',
            dir_name=dir_name,
            s3_dir_name=s3_dir_name,
            ssh_to_workers=True)  # TODO: does this make sense on YARN?

    def _get_step_log_interpretation(self, log_interpretation, step_type):
        """Fetch and interpret the step log."""
        step_id = log_interpretation.get('step_id')

        if not self._read_logs():
            return

        if not step_id:
            log.warning("Can't fetch step log; missing step ID")
            return

        if self._step_type_uses_spark(step_type):
            # Spark also has a "controller" log4j log, but it doesn't
            # contain errors or anything else we need
            #
            # the step log is unlikely to be very much help because
            # Spark on EMR runs in cluster mode. See #2056
            #
            # there's generally only one log (unless the job has been running
            # long enough for log rotation), so use partial=False
            return _interpret_spark_logs(
                self.fs, self._ls_step_stderr_logs(step_id=step_id),
                partial=False)
        else:
            return (
                _interpret_emr_step_syslog(
                    self.fs, self._ls_step_syslogs(step_id=step_id)) or
                _interpret_emr_step_stderr(
                    self.fs, self._ls_step_stderr_logs(step_id=step_id))
            )

    # _ls_step_*() methods are just helpers for _get_step_log_interpretation,
    # so not disabling them if self._read_logs() is false

    def _ls_step_syslogs(self, step_id):
        """Yield step log matches, logging a message for each one."""
        for match in _ls_emr_step_syslogs(
                self.fs, self._stream_step_log_dirs(step_id=step_id),
                step_id=step_id):
            log.info('  Parsing step log: %s' % match['path'])
            yield match

    def _ls_step_stderr_logs(self, step_id):
        """Yield step log matches, logging a message for each one."""
        for match in _ls_emr_step_stderr_logs(
                self.fs, self._stream_step_log_dirs(step_id=step_id),
                step_id=step_id):
            log.info('  Parsing step log: %s' % match['path'])
            yield match

    def _stream_step_log_dirs(self, step_id):
        """Get lists of directories to look for the step log in."""
        return self._stream_log_dirs(
            'step log',
            dir_name=posixpath.join('hadoop', 'steps', step_id),
            s3_dir_name=posixpath.join('steps', step_id))

    def _stream_log_dirs(self, log_desc, dir_name, s3_dir_name,
                         hdfs_dir_name=None,
                         ssh_to_workers=False):
        """Stream log dirs for any kind of log.

        Our general strategy is first, if SSH is enabled, to SSH into the
        master node (and possibly workers, if *ssh_to_workers* is set).

        If this doesn't work, we have to look on S3. If the cluster is
        TERMINATING, we first wait for it to terminate (since that
        will trigger copying logs over).
        """
        if not self._read_logs():
            return

        # used to fetch history logs off HDFS
        if (hdfs_dir_name and
                self.fs.can_handle_path(_DEFAULT_YARN_HDFS_LOG_DIR)):

            hdfs_log_dir = posixpath.join(
                _DEFAULT_YARN_HDFS_LOG_DIR, hdfs_dir_name)

            log.info('Looking for %s in %s...' % (log_desc, hdfs_log_dir))
            yield [hdfs_log_dir]

        if dir_name and self.fs.can_handle_path('ssh:///'):
            ssh_host = self._address_of_master()
            if ssh_host:
                hosts = [ssh_host]
                host_desc = ssh_host
                if ssh_to_workers:
                    try:
                        hosts.extend(self._ssh_worker_hosts())
                        host_desc += ' and task/core nodes'
                    except IOError:
                        log.warning('Could not get worker addresses for %s' %
                                    ssh_host)

                path = posixpath.join(_EMR_LOG_DIR, dir_name)
                log.info('Looking for %s in %s on %s...' % (
                    log_desc, path, host_desc))
                yield ['ssh://%s%s%s' % (
                    ssh_host, '!' + host if host != ssh_host else '',
                    path) for host in hosts]

        # wait for logs to be on S3
        self._wait_for_logs_on_s3()

        s3_dir_name = s3_dir_name or dir_name

        if s3_dir_name and self._s3_log_dir():
            cloud_log_dir = posixpath.join(self._s3_log_dir(), s3_dir_name)
            log.info('Looking for %s in %s...' % (log_desc, cloud_log_dir))
            yield [cloud_log_dir]

    def _ssh_worker_hosts(self):
        """Get the hostnames of all core and task nodes,
        that are currently running, so we can SSH to them through the master
        nodes and read their logs.

        (This currently returns IP addresses rather than full hostnames
        because they're shorter.)
        """
        emr_client = self.make_emr_client()

        instances = _boto3_paginate(
            'Instances', emr_client, 'list_instances',
            ClusterId=self._cluster_id,
            InstanceGroupTypes=['CORE', 'TASK'],
            InstanceStates=['RUNNING'])

        hosts = []

        for instance in instances:
            hosts.append(instance['PrivateIpAddress'])

        return hosts

    def _wait_for_logs_on_s3(self):
        """If the cluster is already terminating, wait for it to terminate,
        so that logs will be transferred to S3.

        Don't print anything unless cluster is in the TERMINATING state.
        """
        cluster = self._describe_cluster()

        if cluster['Status']['State'] in (
                'TERMINATED', 'TERMINATED_WITH_ERRORS'):
            return  # already terminated

        if cluster['Status']['State'] != 'TERMINATING':
            # going to need to wait for logs to get archived to S3

            # "step_num" is just a unique ID for the step; using -1
            # for master node setup script
            if (self._master_node_setup_script_path and
                    self._mns_log_interpretation is None):
                step_num = -1
            else:
                step_num = len(self._log_interpretations)

            # already did this for this step
            if step_num in self._waited_for_logs_on_s3:
                return

            try:
                log.info('Waiting %d minutes for logs to transfer to S3...'
                         ' (ctrl-c to skip)' % _S3_LOG_WAIT_MINUTES)

                if not self.fs.can_handle_path('ssh:///'):
                    log.info(
                        '\n'
                        'To fetch logs immediately next time, set up SSH.'
                        ' See:\n'
                        'https://pythonhosted.org/mrjob/guides'
                        '/emr-quickstart.html#configuring-ssh-credentials\n')

                time.sleep(60 * _S3_LOG_WAIT_MINUTES)
            except KeyboardInterrupt:
                pass

            # do this even if they ctrl-c'ed; don't make them do it
            # for every log for this step
            self._waited_for_logs_on_s3.add(step_num)
            return

        self._wait_for_cluster_to_terminate()

    def counters(self):
        # not using self._pick_counters() because we don't want to
        # initiate a log fetch
        return [_pick_counters(log_interpretation)
                for log_interpretation in self._log_interpretations]

    ### Bootstrapping ###

    def _bootstrap_python(self):
        """Return a (possibly empty) list of parsed commands (in the same
        format as returned by parse_setup_cmd())'"""

        if PY2:
            # Python 2 and pip are basically already installed everywhere
            # (Okay, there's no pip on AMIs prior to 2.4.3, but there's no
            # longer an easy way to get it now that apt-get is broken.)
            return []

        # if bootstrap_python is None, install it for all AMIs up to 4.6.0,
        # and warn if it's an AMI before 3.7.0
        if self._opts['bootstrap_python'] or (
                self._opts['bootstrap_python'] is None and
                not self._image_version_gte('4.6.0')):

            # we have to have at least on AMI 3.7.0. But give it a shot
            if not self._image_version_gte('3.7.0'):
                log.warning(
                    'bootstrapping Python 3 will probably not work on'
                    ' AMIs prior to 3.7.0. For an alternative, see:'
                    ' https://pythonhosted.org/mrjob/guides/emr-bootstrap'
                    '-cookbook.html#installing-python-from-source')

            return [[
                'sudo yum install -y python34 python34-devel python34-pip'
            ]]
        else:
            return []

    def _should_bootstrap_spark(self):
        """Return *bootstrap_spark* option if set; otherwise return
        true if our job has Spark steps."""
        if self._opts['bootstrap_spark'] is None:
            return self._has_spark_steps()
        else:
            return bool(self._opts['bootstrap_spark'])

    def _applications(self, add_spark=True):
        """Returns applications (*applications* option) as a set. Adds
        in ``Hadoop`` and ``Spark`` as needed."""
        applications = set(self._opts['applications'])

        # release_label implies 4.x AMI and later
        if (add_spark and self._should_bootstrap_spark() and
                self._opts['release_label']):
            # EMR allows us to have both "spark" and "Spark" applications,
            # which is probably not what we want
            if not self._has_spark_application():
                applications.add('Spark')

        # patch in "Hadoop" unless applications is empty (e.g. 3.x AMIs)
        if applications:
            # don't add both "Hadoop" and "hadoop"
            if not any(a.lower() == 'hadoop' for a in applications):
                applications.add('Hadoop')

        return applications

    def _bootstrap_actions(self, add_spark=True):
        """Parse *bootstrap_actions* option into dictionaries with
        keys *path*, *args*, adding Spark bootstrap action if needed.

        (This doesn't handle the master bootstrap script.)
        """
        actions = list(self._opts['bootstrap_actions'])

        # no release_label implies AMIs prior to 4.x
        if (add_spark and self._should_bootstrap_spark() and
                not self._opts['release_label']):

            # running this action twice apparently breaks Spark's
            # ability to output to S3 (see #1367)
            if not self._has_spark_install_bootstrap_action():
                actions.append(_3_X_SPARK_BOOTSTRAP_ACTION)

        results = []
        for action in actions:
            args = shlex_split(action)
            if not args:
                raise ValueError('bad bootstrap action: %r' % (action,))

            results.append(dict(path=args[0], args=args[1:]))

        return results

    def _cp_to_local_cmd(self):
        """Command to copy files from the cloud to the local directory."""
        if self._opts['release_label']:
            # on the 4.x AMIs, hadoop isn't yet installed, so use AWS CLI
            return 'aws s3 cp'
        else:
            # on the 2.x and 3.x AMIs, use hadoop
            return 'hadoop fs -copyToLocal'

    def _manifest_download_commands(self):
        return [
            ('s3://*', 'aws s3 cp'),
            ('*://*', 'hadoop fs -copyToLocal'),
        ]

    ### master node setup script ###

    def _create_master_node_setup_script_if_needed(self):
        """Helper for :py:meth:`_add_bootstrap_files_for_upload`.

        If we need a master node setup script and write it into our local
        temp directory. Set self._master_node_setup_script_path.
        """
        # already created
        if self._master_node_setup_script_path:
            return

        # currently, the only thing this script does is upload files
        if not self._master_node_setup_mgr.paths():
            return

        # create script
        path = os.path.join(self._get_local_tmp_dir(), 'mns.sh')
        contents = self._master_node_setup_script_content()

        self._write_script(contents, path, 'master node setup script')

        # the script itself doesn't need to be on the master node, just S3
        self._master_node_setup_script_path = path
        self._upload_mgr.add(path)

    def _master_node_setup_script_content(self):
        """Create the contents of the master node setup script as an
        array of strings.

        (prepare self._master_node_setup_mgr first)
        """
        # TODO: this is very similar to _master_bootstrap_script_content();
        # merge common code
        out = []

        # shebang, etc.
        for line in self._start_of_sh_script():
            out.append(line)
        out.append('')

        # run commands in a block so we can redirect stdout to stderr
        # (e.g. to catch errors from compileall). See #370
        out.append('{')

        # make working dir
        working_dir = self._master_node_setup_working_dir()
        out.append('  mkdir -p %s' % pipes.quote(working_dir))
        out.append('  cd %s' % pipes.quote(working_dir))
        out.append('')

        for name, path in sorted(
                self._master_node_setup_mgr.name_to_path('file').items()):
            uri = self._upload_mgr.uri(path)
            out.append('  %s %s %s' % (
                self._cp_to_local_cmd(), pipes.quote(uri), pipes.quote(name)))
            # imitate Hadoop Distributed Cache
            out.append('  chmod u+rx %s' % pipes.quote(name))

        # at some point we will probably run commands as well (see #1336)

        out.append('} 1>&2')  # stdout -> stderr for ease of error log parsing

        return out

    def _master_node_setup_working_dir(self):
        """Where to place files used by the master node setup script."""
        return '/home/hadoop/%s' % self._job_key

    def _script_runner_jar_uri(self):
        return (
            's3://%s.elasticmapreduce/libs/script-runner/script-runner.jar' %
            self._opts['region'])

    def _build_debugging_step(self):
        if self._opts['release_label']:
            jar = _4_X_COMMAND_RUNNER_JAR
            args = ['state-pusher-script']
        else:
            jar = self._script_runner_jar_uri()
            args = (
                's3://%s.elasticmapreduce/libs/state-pusher/0.1/fetch' %
                self._opts['region'])

        return dict(
            Name='Setup Hadoop Debugging',
            HadoopJarStep=dict(Jar=jar, Args=args),
        )

    def _debug_script_uri(self):
        return (
            's3://%s.elasticmapreduce/libs/state-pusher/0.1/fetch' %
            self._opts['region'])

    ### EMR JOB MANAGEMENT UTILS ###

    def make_persistent_cluster(self):
        if (self._cluster_id):
            raise ValueError(
                'This runner is already associated with cluster ID %s' %
                (self._cluster_id))

        log.info('Creating persistent cluster to run several jobs in...')

        self._add_bootstrap_files_for_upload(persistent=True)
        self._upload_local_files()

        # make sure we can see the files we copied to S3
        self._wait_for_s3_eventual_consistency()

        # don't allow user to call run()
        self._ran_job = True

        self._cluster_id = self._create_cluster(persistent=True)

        return self._cluster_id

    def get_cluster_id(self):
        """Get the ID of the cluster our job is running on, or ``None``."""
        return self._cluster_id

    def _yield_clusters_to_join(self, available_cluster_ids):
        """Get a list of IDs of pooled clusters that this runner can join,
        sorted so that the ones with the greatest CPU capacity come first.

        yields (cluster, when_cluster_described) so we can lock clusters that
        we wish to join (*cluster* is the cluster description and
        *when_cluster_described* is a unix timestamp).
        """
        emr_client = self.make_emr_client()

        for cluster_id in available_cluster_ids:
            if not self._cluster_has_adequate_capacity(cluster_id):
                continue

            # check other things about the cluster that we can't hash
            # (DescribeCluster)
            #
            # save cluster description so we can use it for locking
            when_cluster_described = time.time()
            cluster = emr_client.describe_cluster(
                ClusterId=cluster_id)['Cluster']

            if not self._cluster_description_matches(cluster):
                continue

            yield (cluster, when_cluster_described)

    def _list_cluster_ids_for_pooling(self, created_after=None):
        """Call ListClusters, and collect cluster IDs relevant to pooling.

        Optionally, only list clusters created after *created_after*.

        Returns a dictionary with the following keys:

        available: a list of IDs of clusters that we could join, based on their
                   state and name suffix (pool name and hash, mrjob version).
                   Sorted so that the cluster with the most CPU (based on
                   NormalizedInstanceHours) goes first
        matching: a set of IDs of clusters that have the right name suffix but
                  may or may not be in the right state to join (a superset
                  of *available*)
        in_pool: a set of IDs of clusters that are in the pool we want to join,
                 regardless of their state or pool hash (a superset of
                 *matching*)
        max_created: the latest creation timestamp for *any* cluster listed
                     (so we can call this again to get stats on newly created
                     clusters only)
        """
        # a map from cluster_id to cpu_capacity
        available = {}
        matching = set()
        in_pool = set()
        max_created = None

        name_to_match = self._opts['pool_name']
        suffix_to_match = self._cluster_name_pooling_suffix()

        if self._opts['max_concurrent_steps'] > 1:
            states_to_match = {'RUNNING', 'WAITING'}
        else:
            states_to_match = {'WAITING'}

        emr_client = self.make_emr_client()
        now = _boto3_now()

        # you can't pass CreatedAfter=None to list_clusters()
        list_cluster_kwargs = dict(ClusterStates=_ACTIVE_CLUSTER_STATES)
        if created_after:
            list_cluster_kwargs['CreatedAfter'] = created_after

        log.debug('calling list_clusters(%s)' % ', '.join(
            '%s=%r' % (k, v)
            for k, v in sorted(list_cluster_kwargs.items())))

        for cluster in _boto3_paginate(
                'Clusters', emr_client, 'list_clusters',
                **list_cluster_kwargs):

            cluster_id = cluster['Id']
            log.debug(cluster_id)

            created = cluster['Status']['Timeline']['CreationDateTime']

            if max_created is None or created > max_created:
                max_created = created

            name = _parse_cluster_name_suffix(cluster['Name']).get('pool_name')

            if name != name_to_match:
                continue

            in_pool.add(cluster_id)

            if not cluster['Name'].endswith(suffix_to_match):
                continue

            matching.add(cluster_id)

            if cluster['Status']['State'] not in states_to_match:
                continue

            when_ready = cluster['Status']['Timeline'].get('ReadyDateTime')

            if when_ready:
                hours = max(ceil((now - when_ready).total_seconds() / 3600),
                            1.0)
                cpu_capacity = cluster['NormalizedInstanceHours'] / hours
            else:
                # this probably won't happen, since we only inspect clusters
                # in the WAITING state
                cpu_capacity = 0

            available[cluster_id] = cpu_capacity

        # convert *available* from a dict to a sorted list
        available = sorted(
            available,
            key=lambda c: available[c],
            reverse=True)

        return dict(
            available=available,
            in_pool=in_pool,
            matching=matching,
            max_created=max_created,
        )

    def _cluster_has_adequate_capacity(self, cluster_id):
        """Check if the cluster has an instance group/fleet configuration
        that works as well or better.

        This either calls ``ListInstanceFleets`` or ``ListInstanceGroups``,
        as appropriate
        """
        emr_client = self.make_emr_client()

        if (self._opts['min_available_mb'] or
                self._opts['min_available_virtual_cores']):
            cluster = emr_client.describe_cluster(
                ClusterId=cluster_id)['Cluster']

            host = cluster['MasterPublicDnsName']
            try:
                log.debug('    querying clusterMetrics from %s' % host)
                metrics = self._yrm_get('metrics', host=host)['clusterMetrics']
                log.debug('      metrics: %s' %
                          json.dumps(metrics, sort_keys=True))
            except IOError as ex:
                log.info('    error while getting metrics for cluster %s: %s' %
                         (cluster_id, str(ex)))
                return False

            if metrics['availableMB'] < self._opts['min_available_mb']:
                log.info('    too little memory')
                return False

            if (metrics['availableVirtualCores'] <
                    self._opts['min_available_virtual_cores']):
                log.info('    too few virtual cores')
                return False

            return True
        elif self._opts['instance_fleets']:
            try:
                fleets = list(_boto3_paginate(
                    'InstanceFleets', emr_client, 'list_instance_fleets',
                    ClusterId=cluster_id))
            except botocore.exceptions.ClientError:
                # this shouldn't usually happen because whether a cluster
                # uses instance fleets is in the pool hash
                log.debug('  cluster %s: does not use instance fleets' %
                          cluster_id)
                return False

            return _instance_fleets_satisfy(
                fleets, self._opts['instance_fleets'])
        else:
            try:
                groups = list(_boto3_paginate(
                    'InstanceGroups', emr_client, 'list_instance_groups',
                    ClusterId=cluster_id))
            except botocore.exceptions.ClientError:
                # this shouldn't usually happen because whether a cluster
                # uses instance fleets is in the pool hash
                log.debug(' cluster %s: does not use instance groups' %
                          cluster_id)
                return False

            return _instance_groups_satisfy(groups, self._instance_groups())

    def _cluster_description_matches(self, cluster):
        """Do we want to join the cluster with the given description?"""
        cluster_id = cluster['Id']

        # skip if user specified a key pair and it doesn't match
        if (self._opts['ec2_key_pair'] and
                self._opts['ec2_key_pair'] !=
                cluster['Ec2InstanceAttributes'].get('Ec2KeyName')):
            log.debug('  cluster %s: ec2 key pair mismatch' % cluster_id)
            return False

        # only take persistent clusters
        if cluster['AutoTerminate']:
            log.debug('  cluster %s: not persistent' % cluster_id)
            return False

        # EBS root volume size
        if self._opts['ebs_root_volume_gb']:
            if 'EbsRootVolumeSize' not in cluster:
                log.debug('  cluster %s: EBS root volume size not set' %
                          cluster_id)
                return False
            elif (cluster['EbsRootVolumeSize'] <
                    self._opts['ebs_root_volume_gb']):
                log.debug('  cluster %s: EBS root volume size too small' %
                          cluster_id)
                return False
        else:
            if 'EbsRootVolumeSize' in cluster:
                log.debug('  cluster %s: uses non-default EBS root volume'
                          ' size' % cluster_id)
                return False

        # subnet
        subnet = cluster['Ec2InstanceAttributes'].get('Ec2SubnetId')
        if isinstance(self._opts['subnet'], list):
            matches = (subnet in self._opts['subnet'])
        else:
            # empty subnet is the same as no subnet. see #1931
            matches = (subnet == (self._opts['subnet'] or None))

        if not matches:
            log.debug('  cluster %s: subnet mismatch' % cluster_id)
            return

        # step concurrency
        step_concurrency = cluster.get('StepConcurrencyLevel', 1)
        if step_concurrency > self._opts['max_concurrent_steps']:
            log.debug('  cluster %s: step concurrency level too high' %
                      cluster_id)
            return

        return True

    def _find_cluster(self):
        """Find a cluster that can host this runner. Prefer clusters with more
        compute units. Break ties by choosing cluster with longest idle time.
        Return ``None`` if no suitable clusters exist.
        """
        emr_client = self.make_emr_client()

        start = datetime.now()
        wait_mins = self._opts['pool_wait_minutes']
        timeout_mins = self._opts['pool_timeout_minutes']
        pool_name = self._opts['pool_name']
        max_in_pool = self._opts['max_clusters_in_pool']

        # like sleep() but also raises PoolTimeoutException if we're going to
        # sleep beyond the timeout
        def sleep_or_time_out(seconds):
            if (timeout_mins and (
                    datetime.now() + timedelta(seconds=seconds) >
                    start + timedelta(minutes=timeout_mins))):
                raise PoolTimeoutException(
                    'Unable to join or create a cluster within %d minutes' %
                    timeout_mins)

            time.sleep(seconds)

        log.info('Attempting to find an available cluster...')
        while True:
            cluster_ids = self._list_cluster_ids_for_pooling()

            for cluster, when_cluster_described in (
                    self._yield_clusters_to_join(cluster_ids['available'])):
                cluster_id = cluster['Id']
                step_concurrency_level = cluster['StepConcurrencyLevel']

                log.info('  Attempting to join cluster %s' % cluster_id)
                lock_acquired = _attempt_to_lock_cluster(
                    emr_client, cluster_id, self._job_key,
                    cluster=cluster,
                    when_cluster_described=when_cluster_described)

                if lock_acquired:
                    return cluster_id, step_concurrency_level

            keep_waiting = (
                datetime.now() < start + timedelta(minutes=wait_mins))

            # if we haven't exhausted pool_wait_minutes, and there are
            # clusters we might eventually join, sleep and try again
            if keep_waiting and cluster_ids['matching']:
                log.info('No clusters in pool %r are available. Checking again'
                         ' in %d seconds...' % (
                             pool_name, int(_POOLING_SLEEP_INTERVAL)))
                sleep_or_time_out(_POOLING_SLEEP_INTERVAL)
                continue

            # implement max_clusters_in_pool
            if max_in_pool:
                num_in_pool = len(cluster_ids['in_pool'])

                log.info('  %d cluster%s in pool %r (max. is %d)' % (
                    num_in_pool, _plural(num_in_pool), pool_name, max_in_pool))

                if num_in_pool >= max_in_pool:
                    log.info('Checking again in %d seconds...' % (
                        _POOLING_SLEEP_INTERVAL))
                    sleep_or_time_out(_POOLING_SLEEP_INTERVAL)
                    continue

            # to avoid race conditions, double-check the clusters in the pool
            # if we need to satisfy max_clusters_in_pool or are trying to
            # bypass pool_wait_minutes
            if max_in_pool or (keep_waiting and not cluster_ids['matching']):
                jitter_seconds = randint(0, self._opts['pool_jitter_seconds'])

                log.info('  waiting %d seconds and double-checking for'
                         ' newly created clusters...' % jitter_seconds)
                sleep_or_time_out(jitter_seconds)

                new_cluster_ids = self._list_cluster_ids_for_pooling(
                    created_after=cluster_ids['max_created'])

                new_num_in_pool = len(
                    cluster_ids['in_pool'] | new_cluster_ids['in_pool'])

                log.info('    %d cluster%s in pool' % (
                    new_num_in_pool, _plural(new_num_in_pool)))

                if ((not max_in_pool or new_num_in_pool < max_in_pool) and
                        (not keep_waiting or not new_cluster_ids['matching'])):

                    # allow creating a new cluster
                    return None, None

                log.info('Checking again in %d seconds...' % (
                    _POOLING_SLEEP_INTERVAL))
                sleep_or_time_out(_POOLING_SLEEP_INTERVAL)

                continue

            # pool_wait_minutes is exhausted and max_clusters_in_pool is not
            # set, so create a new cluster
            return None, None

        # (defensive programming, in case we break out of the loop)
        return None, None

    def _pool_hash_dict(self):
        """A dictionary of information that must be matched exactly to
        join a pooled cluster (other than mrjob version and pool name).

        The format of this dictionary may change between mrjob versions.
        """
        # this can be expensive because we have to read every file used in
        # bootstrapping, so cache it
        if not self._pool_hash_dict_cached:
            d = {}

            # additional_emr_info
            d['additional_emr_info'] = self._opts['additional_emr_info']

            # applications
            # (these are case-insensitive)
            d['applications'] = sorted(a.lower() for a in self._applications())

            # bootstrapping

            # bootstrap_actions
            d['bootstrap_actions'] = self._bootstrap_actions()

            # bootstrap_file_md5sums
            d['bootstrap_file_md5sums'] = {
                name: self.fs.md5sum(path)
                for name, path
                in self._bootstrap_dir_mgr.name_to_path().items()
                if path != self._mrjob_zip_path
            }

            # bootstrap_without_paths
            # original path doesn't matter, just contents (above) and name
            d['bootstrap_without_paths'] = [
                [
                    dict(type=x['type'],
                         name=self._bootstrap_dir_mgr.name(**x))
                    if isinstance(x, dict) else x
                    for x in cmd
                ]
                for cmd in self._bootstrap
            ]

            # emr_configurations
            d['emr_configurations'] = self._emr_configurations()

            # image_id
            d['image_id'] = self._opts['image_id']

            # instance_collection_type
            # no way to compare instance groups with instance fleets
            # so make it part of the hash
            d['instance_collection_type'] = (
                'INSTANCE_FLEET' if self._opts['instance_fleets']
                else 'INSTANCE_GROUP'
            )

            # release_label
            # use e.g. emr-2.4.9 for 2.x/3.x AMIs, even though the API wouldn't
            d['release_label'] = (self._opts['release_label'] or
                                  'emr-' + self._opts['image_version'])

            self._pool_hash_dict_cached = d

        return self._pool_hash_dict_cached

    def _pool_hash(self):
        hash_dict = self._pool_hash_dict()

        hash_json = json.dumps(hash_dict, sort_keys=True)
        if not isinstance(hash_json, bytes):
            hash_json = hash_json.encode('utf_8')

        m = hashlib.md5()
        m.update(hash_json)
        return m.hexdigest()

    def _cluster_name_pooling_suffix(self):
        """Extra info added to the cluster name, for pooling."""
        if not self._opts['pool_clusters']:
            return ''
        else:
            return _cluster_name_suffix(
                self._pool_hash(), self._opts['pool_name'])

    ### EMR-specific Stuff ###

    def make_emr_client(self):
        """Create a :py:mod:`boto3` EMR client.

        :return: a :py:class:`botocore.client.EMR` wrapped in a
                :py:class:`mrjob.retry.RetryWrapper`
        """
        # ...which is then wrapped in bacon! Mmmmm!
        if boto3 is None:
            raise ImportError('You must install boto3 to connect to EMR')

        raw_emr_client = boto3.client(
            'emr',
            aws_access_key_id=self._opts['aws_access_key_id'],
            aws_secret_access_key=self._opts['aws_secret_access_key'],
            aws_session_token=self._opts['aws_session_token'],
            endpoint_url=_endpoint_url(self._opts['emr_endpoint']),
            region_name=self._opts['region'],
        )

        # #1799: don't retry faster than EMR checks the API
        return _wrap_aws_client(raw_emr_client,
                                min_backoff=self._opts['check_cluster_every'])

    def _describe_cluster(self):
        emr_client = self.make_emr_client()
        return emr_client.describe_cluster(
            ClusterId=self._cluster_id)['Cluster']

    def get_hadoop_version(self):
        return self._get_app_versions().get('hadoop')

    def get_image_version(self):
        """Get the version of the AMI that our cluster is running, or ``None``.
        """
        return self._get_cluster_info('image_version')

    def _address_of_master(self):
        """Get the address of the master node so we can SSH to it"""
        return self._get_cluster_info('master_public_dns')

    def _master_private_ip(self):
        """Get the internal ("private") address of the master node, so we
        can direct our SSH tunnel to it."""
        return self._get_cluster_info('master_private_ip')

    def _get_app_versions(self):
        """Returns a map from lowercase app name to version for our cluster.

        For apps other than Hadoop, this only works for AMI 4.x and later.
        """
        return self._get_cluster_info('app_versions')

    def _get_collection_type(self):
        """Return the collection type of the cluster (either
        ``'INSTANCE_FLEET'`` or ``'INSTANCE_GROUP'``)."""
        return self._get_cluster_info('collection_type')

    def _get_cluster_info(self, key):
        if not self._cluster_id:
            return None

        cache = self._cluster_to_cache[self._cluster_id]

        if not cache.get(key):
            if key == 'master_private_ip':
                self._store_master_instance_info()
            else:
                self._store_cluster_info()

        return cache.get(key)

    def _store_cluster_info(self):
        """Describe our cluster, and cache image_version, hadoop_version,
        and master_public_dns"""
        if not self._cluster_id:
            raise ValueError('cluster has not yet been created')

        cache = self._cluster_to_cache[self._cluster_id]

        cluster = self._describe_cluster()

        # AMI version might be in RunningAMIVersion (2.x, 3.x)
        # or ReleaseLabel (4.x)
        cache['image_version'] = cluster.get('RunningAmiVersion')
        if not cache['image_version']:
            release_label = cluster.get('ReleaseLabel')
            if release_label:
                cache['image_version'] = release_label.lstrip('emr-')

        cache['app_versions'] = dict(
            (a['Name'].lower(), a.get('Version'))
            for a in cluster['Applications'])

        cache['collection_type'] = cluster.get(
            'InstanceCollectionType', 'INSTANCE_GROUP')

        if cluster['Status']['State'] in ('RUNNING', 'WAITING'):
            cache['master_public_dns'] = cluster['MasterPublicDnsName']

    def _store_master_instance_info(self):
        """List master instance for our cluster, and cache
        master_private_ip."""
        if not self._cluster_id:
            raise ValueError('cluster has not yet been created')

        cache = self._cluster_to_cache[self._cluster_id]

        emr_client = self.make_emr_client()

        instances = emr_client.list_instances(
            ClusterId=self._cluster_id,
            InstanceGroupTypes=['MASTER'])['Instances']

        if not instances:
            return

        master = instances[0]

        # can also get private DNS and public IP/DNS, but we don't use this
        master_private_ip = master.get('PrivateIpAddress')
        if master_private_ip:  # may not have been assigned yet
            cache['master_private_ip'] = master_private_ip

    def make_ec2_client(self):
        """Create a :py:mod:`boto3` EC2 client.

        :return: a :py:class:`botocore.client.EC2` wrapped in a
                :py:class:`mrjob.retry.RetryWrapper`
        """
        if boto3 is None:
            raise ImportError('You must install boto3 to connect to EC2')

        raw_ec2_client = boto3.client(
            'ec2',
            aws_access_key_id=self._opts['aws_access_key_id'],
            aws_secret_access_key=self._opts['aws_secret_access_key'],
            aws_session_token=self._opts['aws_session_token'],
            endpoint_url=_endpoint_url(self._opts['ec2_endpoint']),
            region_name=self._opts['region'],
        )

        return _wrap_aws_client(raw_ec2_client)

    def make_iam_client(self):
        """Create a :py:mod:`boto3` IAM client.

        :return: a :py:class:`botocore.client.IAM` wrapped in a
                :py:class:`mrjob.retry.RetryWrapper`
        """
        if boto3 is None:
            raise ImportError('You must install boto3 to connect to IAM')

        # special logic for setting IAM endpoint (which you don't usually
        # want to do, because IAM is regionless).
        endpoint_url = _endpoint_url(self._opts['iam_endpoint'])
        if endpoint_url:
            # keep boto3 from loading a nonsensical region name from configs
            # (see https://github.com/boto/boto3/issues/985)
            region_name = _DEFAULT_AWS_REGION
            log.debug('creating IAM client to %s' % endpoint_url)
        else:
            region_name = None
            log.debug('creating IAM client')

        raw_iam_client = boto3.client(
            'iam',
            aws_access_key_id=self._opts['aws_access_key_id'],
            aws_secret_access_key=self._opts['aws_secret_access_key'],
            aws_session_token=self._opts['aws_session_token'],
            endpoint_url=endpoint_url,
            region_name=region_name,
        )

        return _wrap_aws_client(raw_iam_client)

    # Spark

    def _uses_spark(self):
        """Does this runner use Spark, based on steps, bootstrap actions,
        and EMR applications? If so, we'll need more memory."""
        return (self._has_spark_steps() or
                self._has_spark_install_bootstrap_action() or
                self._has_spark_application() or
                self._opts['bootstrap_spark'])

    def _has_spark_install_bootstrap_action(self):
        """Does it look like this runner has a spark bootstrap install
        action set? (Anything ending in "/install-spark" counts.)"""
        return any(ba['path'].endswith('/install-spark')
                   for ba in self._bootstrap_actions(add_spark=False))

    def _has_spark_application(self):
        """Does this runner have "Spark" in its *applications* option?"""
        return any(a.lower() == 'spark'
                   for a in self._applications(add_spark=False))

    def _check_cluster_spark_support(self):
        """Issue a warning if our cluster doesn't support Spark.

        This should only be called if you are going to run one or more
        Spark steps.
        """
        message = self._cluster_spark_support_warning()
        if message:
            log.warning(message)

    def _cluster_spark_support_warning(self):
        """Helper for _check_cluster_spark_support()."""
        image_version = self.get_image_version()

        if not version_gte(image_version, _MIN_SPARK_AMI_VERSION):
            suggested_version = (
                _MIN_SPARK_AMI_VERSION if PY2 else _MIN_SPARK_PY3_AMI_VERSION)
            return ('  AMI version %s does not support Spark;\n'
                    '  (try --image-version %s or later)' % (
                        image_version, suggested_version))

        if not version_gte(image_version, _MIN_SPARK_PY3_AMI_VERSION):
            if PY2:
                # even though this version of Spark "works" with Python 2,
                # it doesn't work well
                return ('  AMI version %s has an old version of Spark\n'
                        ' and does not correctly determine when a Spark'
                        ' job has failed\n'
                        'Try --image-version %s or later)' % (
                            image_version, _MIN_SPARK_PY3_AMI_VERSION))
            else:
                # this version of Spark doesn't support Python 3 at all!
                return ('  AMI version %s does not support Python 3 on Spark\n'
                        '  (try --image-version %s or later)' % (
                            image_version, _MIN_SPARK_PY3_AMI_VERSION))

        emr_client = self.make_emr_client()

        too_small_msg = ('  instance type %s is too small for Spark;'
                         ' your job may stall forever')

        if self._get_collection_type() == 'INSTANCE_FLEET':
            fleets = list(_boto3_paginate(
                'InstanceFleets', emr_client, 'list_instance_fleets',
                ClusterId=self.get_cluster_id()))

            for fleet in fleets:
                # master doesn't matter if it's not running tasks
                if fleet['InstanceFleetType'] == 'MASTER' and len(fleets) > 1:
                    continue

                for spec in fleet['InstanceTypeSpecifications']:
                    mem = EC2_INSTANCE_TYPE_TO_MEMORY.get(spec['InstanceType'])
                    if mem and mem < _MIN_SPARK_INSTANCE_MEMORY:
                        return (too_small_msg % spec['InstanceType'])
        else:
            # instance groups
            igs = list(_boto3_paginate(
                'InstanceGroups', emr_client, 'list_instance_groups',
                ClusterId=self.get_cluster_id()))

            for ig in igs:
                # master doesn't matter if it's not running tasks
                if ig['InstanceGroupType'] == 'MASTER' and len(igs) > 1:
                    continue

                mem = EC2_INSTANCE_TYPE_TO_MEMORY.get(ig['InstanceType'])
                if mem and mem < _MIN_SPARK_INSTANCE_MEMORY:
                    return (too_small_msg % ig['InstanceType'])

        return None

    def _cmdenv(self):
        env = super(EMRJobRunner, self)._cmdenv()

        return combine_dicts(self._docker_cmdenv(), env)

    def _emr_configurations(self):
        # don't keep two configs with the same Classification (#2097)
        return _deduplicate_emr_configurations(
            self._docker_emr_configurations() +
            self._opts['emr_configurations']
        )

    def _docker_image(self):
        """Special-case the "library" registry which is implied on Docker Hub
        but needs to be specified explicitly on EMR."""
        image = self._opts['docker_image']

        if not image:
            return None
        elif '/' in image:
            return image
        else:
            return 'library/' + image

    def _docker_registry(self):
        """Infer the trusted docker registry from the docker image."""
        image = self._docker_image()

        if not image:
            return None
        else:
            return image.split('/')[0]

    def _docker_cmdenv(self):
        image = self._docker_image()

        if not image:
            return {}

        env = dict(
            YARN_CONTAINER_RUNTIME_TYPE='docker',
            YARN_CONTAINER_RUNTIME_DOCKER_IMAGE=image,
        )

        if self._opts['docker_client_config']:
            env['YARN_CONTAINER_RUNTIME_DOCKER_CLIENT_CONFIG'] = (
                self._opts['docker_client_config'])

        if self._opts['docker_mounts']:
            env['YARN_CONTAINER_RUNTIME_DOCKER_MOUNTS'] = ','.join(
                self._opts['docker_mounts'])

        return env

    def _docker_emr_configurations(self):
        registry = self._docker_registry()

        if not registry:
            return []

        registries = ','.join(['local', registry])

        return [
            dict(
                Classification='container-executor',
                Configurations=[
                    dict(
                        Classification='docker',
                        Properties={
                            'docker.trusted.registries': registries,
                            'docker.privileged-containers.registries': (
                                registries),
                        },
                    ),
                ],
                Properties={},
            ),
        ]

    def _yrm_get(self, path, host=None, port=None, timeout=None):
        """Use curl to perform an HTTP GET on the given path on the
        YARN Resource Manager. Either return decoded JSON from the call,
        or raise an IOError

        *path* should not start with a '/'

        More info on the YARN REST API can be found here:

        https://hadoop.apache.org/docs/current/hadoop-yarn/
            hadoop-yarn-site/ResourceManagerRest.html
        """
        if host is None:
            host = self._address_of_master()

        if port is None:
            port = _YARN_RESOURCE_MANAGER_PORT

        if timeout is None:
            timeout = _YARN_API_TIMEOUT

        # using urljoin() to avoid a double / when joining host/port with path
        yrm_url = urljoin(
            'http://{}:{:d}'.format(host, port),
            '{}/{}'.format(_YRM_BASE_PATH, path)
        )

        curl_args = [
            'curl',  # always available on EMR
            '-fsS',  # fail on HTTP errors, print errors only to stderr
            '-m', str(timeout),  # timeout after 20 seconds
            yrm_url,
        ]

        stdout, stderr = self.fs.ssh._ssh_run(host, curl_args)

        return json.loads(to_unicode(stdout))


def _get_job_steps(emr_client, cluster_id, job_key):
    """Efficiently fetch steps for a particular mrjob run from the EMR API.

    :param emr_client: a boto3 EMR client. See
                       :py:meth:`~mrjob.emr.EMRJobRunner.make_emr_client`
    :param cluster_id: ID of EMR cluster to fetch steps from. See
                       :py:meth:`~mrjob.emr.EMRJobRunner.get_cluster_id`
    :param job_key: Unique key for a mrjob run. See
                    :py:meth:`~mrjob.runner.MRJobRunner.get_job_key`
    """
    steps = []

    for step in _boto3_paginate('Steps', emr_client, 'list_steps',
                                ClusterId=cluster_id):
        if step['Name'].startswith(job_key):
            steps.append(step)
        elif steps:
            # all steps for job will be together, so stop
            # when we find a non-job step
            break

    return list(reversed(list(steps)))


def _get_reason(cluster_or_step):
    """Get state change reason message."""
    # StateChangeReason is {} before the first state change
    return cluster_or_step['Status']['StateChangeReason'].get('Message', '')


def _deduplicate_emr_configurations(emr_configurations):
    """Takes the value of the *emr_configurations* opt, and ensures that
    later configs overwrite earlier ones with the same Classification.

    Additionally, any configs that contain empty or unset Properties
    and Configurations will be removed (this is a way of deleting
    existing config dicts without replacing them).

    You can assume that all config dicts have run through
    _fix_configuration_opt()
    """
    results = OrderedDict()

    for c in emr_configurations:
        results[c['Classification']] = c

    return [c for c in results.values() if
            c['Properties'] or c.get('Configurations')]


def _fix_configuration_opt(c):
    """Return copy of *c* with *Properties* is always set
    (defaults to {}) and with *Configurations* is not set if empty.
    Convert all values to strings.

    Raise exception on more serious problems (extra fields, wrong data
    type, etc).

    This allows us to match configurations against the API, *and* catches bad
    configurations before they result in cryptic API errors.
    """
    if not isinstance(c, dict):
        raise TypeError('configurations must be dicts, not %r' % (c,))

    c = dict(c)  # make a copy

    # extra keys
    extra_keys = (
        set(c) - set(['Classification', 'Configurations', 'Properties']))
    if extra_keys:
        raise ValueError('configuration opt has extra keys: %s' % ', '.join(
            sorted(extra_keys)))

    # Classification
    if 'Classification' not in c:
        raise ValueError('configuration opt has no Classification')

    if not isinstance(c['Classification'], string_types):
        raise TypeError('Classification must be string')

    # Properties
    c.setdefault('Properties', {})
    if not isinstance(c['Properties'], dict):
        raise TypeError('Properties must be a dict')

    c['Properties'] = dict(
        (str(k), str(v)) for k, v in c['Properties'].items())

    # sub-Configurations
    if 'Configurations' in c:
        if c['Configurations']:
            if not isinstance(c['Configurations'], list):
                raise TypeError('Configurations must be a list')
            # recursively fix subconfigurations
            c['Configurations'] = [
                _fix_configuration_opt(sc) for sc in c['Configurations']]
        else:
            # don't keep empty configurations around
            del c['Configurations']

    return c


def _fix_subnet_opt(subnet):
    """Return either None, a string, or a list with at least two items."""
    if subnet is None:
        return None

    if isinstance(subnet, string_types):
        return subnet

    subnet = list(subnet)
    if len(subnet) == 1:
        return subnet[0]
    else:
        return subnet


def _build_instance_group(role, instance_type, num_instances, bid_price):
    """Helper method for creating instance groups. For use when
    creating a cluster using a list of InstanceGroups

        - role is either 'MASTER', 'CORE', or 'TASK'.
        - instance_type is an EC2 instance type
        - count is an int
        - bid_price is a number, a string, or None. If None,
          this instance group will be use the ON-DEMAND market
          instead of the SPOT market.
    """
    if role not in _INSTANCE_ROLES:
        raise ValueError

    if not instance_type:
        raise ValueError

    if not num_instances:
        raise ValueError

    ig = dict(
        InstanceCount=num_instances,
        InstanceRole=role,
        InstanceType=instance_type,
        Market='ON_DEMAND',
        Name=role.lower(),  # just name the groups "core", "master", and "task"
    )

    if bid_price:
        ig['Market'] = 'SPOT'
        ig['BidPrice'] = str(bid_price)  # must be a string

    return ig


def _plural(n):
    """Utility for logging messages"""
    if n == 1:
        return ''
    else:
        return 's'
