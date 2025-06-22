# -*- coding: utf-8 -*-
# Copyright 2016 Google Inc. and Yelp
# Copyright 2017 Yelp
# Copyright 2018 Google Inc. and Yelp
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
import json
import logging
import time
import re
from io import BytesIO
from os import environ

try:
    import google.auth
    import google.cloud.dataproc_v1beta2
    import google.cloud.dataproc_v1beta2.types
    import google.cloud.logging
    import google.api_core.exceptions
    import google.api_core.grpc_helpers
except:
    google = None

import mrjob
from mrjob.cloud import HadoopInTheCloudJobRunner
from mrjob.compat import map_version
from mrjob.conf import combine_dicts
from mrjob.fs.composite import CompositeFilesystem
from mrjob.fs.gcs import GCSFilesystem
from mrjob.fs.gcs import is_gcs_uri
from mrjob.fs.local import LocalFilesystem
from mrjob.logs.counters import _pick_counters
from mrjob.logs.errors import _log_probable_cause_of_failure
from mrjob.logs.mixin import LogInterpretationMixin
from mrjob.logs.task import _parse_task_stderr
from mrjob.logs.task import _parse_task_syslog_records
from mrjob.logs.step import _interpret_new_dataproc_step_stderr
from mrjob.parse import is_uri
from mrjob.py2 import PY2
from mrjob.py2 import string_types
from mrjob.py2 import to_unicode
from mrjob.runner import _blank_out_conflicting_opts
from mrjob.setup import UploadDirManager
from mrjob.step import StepFailedException
from mrjob.util import random_identifier

log = logging.getLogger(__name__)

_DEFAULT_GCE_REGION = 'us-west1'

_DEFAULT_ENDPOINT = 'dataproc.googleapis.com:443'

_DATAPROC_MIN_WORKERS = 2
_GCE_API_VERSION = 'v1'

_DEFAULT_INSTANCE_TYPE = 'n1-standard-1'

# default imageVersion to use on Dataproc. This may be updated with each
# version of mrjob
_DEFAULT_IMAGE_VERSION = '1.3'
_DEFAULT_CHECK_CLUSTER_EVERY = 10.0
_DEFAULT_CLOUD_FS_SYNC_SECS = 5.0
_DEFAULT_CLOUD_TMP_DIR_OBJECT_TTL_DAYS = 90

# job state matcher enum
# use this to only find active jobs. (2 for NON_ACTIVE, but we don't use that)
_STATE_MATCHER_ACTIVE = 1

# Dataproc images where Hadoop version changed (we use map_version() on this)
#
# This will need to be updated by hand if we want it to be fully accurate
# (it doesn't really matter to mrjob though, which only cares about
# major version)
#
# See https://cloud.google.com/dataproc/docs/concepts/dataproc-versions
# for the full list.
_DATAPROC_IMAGE_TO_HADOOP_VERSION = {
    '0.1': '2.7.1',
    '1.0': '2.7.2',
    '1.1': '2.7.7',
    '1.2': '2.8.5',
    '1.3': '2.9.2',
}

_HADOOP_STREAMING_JAR_URI = (
    'file:///usr/lib/hadoop-mapreduce/hadoop-streaming.jar')

# TODO - mtai @ davidmarin - Re-implement logs parsing?  See dataproc metainfo and driver output - ask Dennis Huo for more details  # noqa
# 'gs://dataproc-801485be-0997-40e7-84a7-00926031747c-us/google-cloud-dataproc-metainfo/8b76d95e-ebdc-4b81-896d-b2c5009b3560/jobs/mr_most_used_word-taim-20160228-172000-034993---Step-2-of-2/driveroutput'  # noqa

_GCP_CLUSTER_NAME_REGEX = '(?:[a-z](?:[-a-z0-9]{0,53}[a-z0-9])?).'

# on Dataproc, the resource manager is always at 8088. Tunnel to the master
# node's own hostname, not localhost.
_SSH_TUNNEL_CONFIG = dict(
    localhost=False,
    name='resource manager',
    path='/cluster',
    port=8088,
)

# used to match log entries that tell us if a container exited
_CONTAINER_EXECUTOR_CLASS_NAME = (
    'org.apache.hadoop.yarn.server.nodemanager.DefaultContainerExecutor')

# used to determine which containers exited with nonzero status
_CONTAINER_EXIT_RE = re.compile(
    r'Exit code from container (?P<container_id>\w+)'
    r' is ?: (?P<returncode>\d+)')

_TRACEBACK_EXCEPTION_RE = re.compile(r'\w+: .*$')

_STDERR_LOG4J_WARNING = re.compile(
    r'.*(No appenders could be found for logger'
    r'|Please initialize the log4j system'
    r'|See http://logging.apache.org/log4j)')

# this is equivalent to full permission
_FULL_SCOPE = 'https://www.googleapis.com/auth/cloud-platform'


# convert enum values to strings (e.g. 'RUNNING')

def _cluster_state_name(state_value):
    return google.cloud.dataproc_v1beta2.types.ClusterStatus.State.Name(
        state_value)


def _job_state_name(state_value):
    return google.cloud.dataproc_v1beta2.types.JobStatus.State.Name(
        state_value)


########## BEGIN - Helper fxns for _cluster_create_kwargs ##########
def _gcp_zone_uri(project, zone):
    return (
        'https://www.googleapis.com/compute/%(gce_api_version)s/projects/'
        '%(project)s/zones/%(zone)s' % dict(
            gce_api_version=_GCE_API_VERSION, project=project, zone=zone))


def _gcp_instance_group_config(
        project, zone, count, instance_type, is_preemptible=False):
    if zone:
        zone_uri = _gcp_zone_uri(project, zone)
        machine_type = "%(zone_uri)s/machineTypes/%(machine_type)s" % dict(
            zone_uri=zone_uri, machine_type=instance_type)
    else:
        machine_type = instance_type

    return dict(
        num_instances=count,
        machine_type_uri=machine_type,
        is_preemptible=is_preemptible
    )
########## END -  Helper fxns for _cluster_create_kwargs ###########


def _wait_for(msg, sleep_secs):
    log.info("Waiting for %s - sleeping %.1f second(s)", msg, sleep_secs)
    time.sleep(sleep_secs)


def _cleanse_gcp_job_id(job_id):
    return re.sub(r'[^a-zA-Z0-9_\-]', '-', job_id)


def _check_and_fix_fs_dir(gcs_uri):
    """Helper for __init__"""
    # TODO - mtai @ davidmarin - push this to fs/*.py
    if not is_gcs_uri(gcs_uri):
        raise ValueError('Invalid GCS URI: %r' % gcs_uri)
    if not gcs_uri.endswith('/'):
        gcs_uri += '/'

    return gcs_uri


def _zone_to_region(zone):
    """Convert a zone (like us-west1-b) to the corresponding region
    (like us-west1)."""
    # See https://cloud.google.com/compute/docs/regions-zones/#identifying_a_region_or_zone  # noqa
    return '-'.join(zone.split('-')[:-1])


class DataprocException(Exception):
    pass


class DataprocJobRunner(HadoopInTheCloudJobRunner, LogInterpretationMixin):
    """Runs an :py:class:`~mrjob.job.MRJob` on Google Cloud Dataproc.
    Invoked when you run your job with ``-r dataproc``.

    :py:class:`DataprocJobRunner` runs your job in an Dataproc cluster, which
    is basically a temporary Hadoop cluster.

    Input, support, and jar files can be either local or on GCS; use
    ``gs://...`` URLs to refer to files on GCS.

    This class has some useful utilities for talking directly to GCS and
    Dataproc, so you may find it useful to instantiate it without a script::

        from mrjob.dataproc import DataprocJobRunner
        ...
    """
    alias = 'dataproc'

    OPT_NAMES = HadoopInTheCloudJobRunner.OPT_NAMES | {
        'cluster_properties',
        'core_instance_config',
        'gcloud_bin',
        'master_instance_config',
        'network',
        'project_id',
        'service_account',
        'service_account_scopes',
        'subnet',
        'task_instance_config',
    }

    # no Spark support yet (see #1765)
    _STEP_TYPES = {'jar', 'streaming'}

    def __init__(self, **kwargs):
        """:py:class:`~mrjob.dataproc.DataprocJobRunner` takes the same
        arguments as
        :py:class:`~mrjob.runner.MRJobRunner`, plus some additional options
        which can be defaulted in :ref:`mrjob.conf <mrjob.conf>`.
        """
        super(DataprocJobRunner, self).__init__(**kwargs)

        # check for library support
        if google is None:
            raise ImportError(
                'You must install google-cloud-logging and '
                'google-cloud-storage to connect to Dataproc')

        # Dataproc requires a master and >= 2 core instances
        # num_core_instances refers ONLY to number of CORE instances and does
        # NOT include the required 1 instance for master
        # In other words, minimum cluster size is 3 machines, 1 master and 2
        # "num_core_instances" workers
        if self._opts['num_core_instances'] < _DATAPROC_MIN_WORKERS:
            raise DataprocException(
                'Dataproc expects at LEAST %d workers' % _DATAPROC_MIN_WORKERS)

        if (self._opts['core_instance_type'] !=
                self._opts['task_instance_type']):
            raise DataprocException(
                'Dataproc v1 expects core/task instance types to be identical')

        # see #1820
        if self._opts['image_id']:
            log.warning('mrjob does not yet support custom machine images'
                        ' on Dataproc')

        # load credentials and project ID
        self._credentials, auth_project_id = google.auth.default(
            scopes=[_FULL_SCOPE])  # needed for $GOOGLE_APPLICATION_CREDENTIALS

        self._project_id = self._opts['project_id'] or auth_project_id

        if not self._project_id:
            raise DataprocException(
                'project_id must be set. Use --project_id or'
                ' set $GOOGLE_CLOUD_PROJECT')

        self._fix_zone_and_region_opts()

        if self._opts['service_account_scopes']:
            self._opts['service_account_scopes'] = [
                _fully_qualify_scope_uri(s)
                for s in self._opts['service_account_scopes']
            ]

        # cluster_id can be None here
        self._cluster_id = self._opts['cluster_id']

        self._api_client = None
        self._gcs_fs = None
        self._fs = None

        # BEGIN - setup directories
        base_tmpdir = self._get_tmpdir(self._opts['cloud_tmp_dir'])

        self._cloud_tmp_dir = _check_and_fix_fs_dir(base_tmpdir)

        # use job key to make a unique tmp dir
        self._job_tmpdir = self._cloud_tmp_dir + self._job_key + '/'

        # pick/validate output dir
        if self._output_dir:
            self._output_dir = _check_and_fix_fs_dir(self._output_dir)
        else:
            self._output_dir = self._job_tmpdir + 'output/'
        # END - setup directories

        # manage local files that we want to upload to GCS. We'll add them
        # to this manager just before we need them.
        fs_files_dir = self._job_tmpdir + 'files/'
        self._upload_mgr = UploadDirManager(fs_files_dir)

        # when did our particular task start?
        self._dataproc_job_start = None

        # init hadoop, ami version caches
        self._image_version = None
        self._hadoop_version = None

        # map driver_output_uri to a dict with the keys:
        # log_uri: uri of file we're reading from
        # pos: position in file
        # buffer: bytes read from file already
        self._driver_output_state = {}

        # This will be filled by _run_steps()
        # NOTE - log_interpretations will be empty except job_id until we
        # parse task logs
        self._log_interpretations = []

    def _fix_zone_and_region_opts(self):
        """Ensure that exactly one of region and zone is set."""
        if self._opts['region'] and self._opts['zone']:
            log.warning('you do not need to set region if you set zone')
            self._opts['region'] = None
            return

        if not (self._opts['region'] or self._opts['zone']):
            if environ.get('CLOUDSDK_COMPUTE_ZONE'):
                self._opts['zone'] = environ['CLOUDSDK_COMPUTE_ZONE']
            elif environ.get('CLOUDSDK_COMPUTE_REGION'):
                self._opts['region'] = environ['CLOUDSDK_COMPUTE_REGION']
            else:
                self._opts['region'] = _DEFAULT_GCE_REGION

    @classmethod
    def _default_opts(cls):
        return combine_dicts(
            super(DataprocJobRunner, cls)._default_opts(),
            dict(
                bootstrap_python=True,
                check_cluster_every=_DEFAULT_CHECK_CLUSTER_EVERY,
                cleanup=['CLUSTER', 'JOB', 'LOCAL_TMP'],
                cloud_fs_sync_secs=_DEFAULT_CLOUD_FS_SYNC_SECS,
                image_version=_DEFAULT_IMAGE_VERSION,
                instance_type=_DEFAULT_INSTANCE_TYPE,
                master_instance_type=_DEFAULT_INSTANCE_TYPE,
                num_core_instances=_DATAPROC_MIN_WORKERS,
                num_task_instances=0,
            )
        )

    def _combine_opts(self, opt_list):
        """Blank out conflicts between *network*/*subnet* and
        *region*/*zone*."""
        opt_list = _blank_out_conflicting_opts(opt_list, ['region', 'zone'])
        opt_list = _blank_out_conflicting_opts(opt_list, ['network', 'subnet'])

        # now combine opts, with region/zone blanked out
        return super(DataprocJobRunner, self)._combine_opts(opt_list)

    @property
    def cluster_client(self):
        return google.cloud.dataproc_v1beta2.ClusterControllerClient(
            **self._client_create_kwargs())

    @property
    def job_client(self):
        return google.cloud.dataproc_v1beta2.JobControllerClient(
            **self._client_create_kwargs())

    @property
    def logging_client(self):
        return google.cloud.logging.Client(credentials=self._credentials,
                                           project=self._project_id)

    def _client_create_kwargs(self):
        if self._opts['region']:
            endpoint = '%s-%s' % (self._opts['region'], _DEFAULT_ENDPOINT)
            return dict(
                channel=google.api_core.grpc_helpers.create_channel(
                    endpoint, credentials=self._credentials))
        else:
            return dict(credentials=self._credentials)

    @property
    def api_client(self):
        raise NotImplementedError(
            '"api_client" was disabled in v0.6.2. Use "cluster_client"'
            ' or "job_client" instead.')

    @property
    def fs(self):
        """:py:class:`~mrjob.fs.base.Filesystem` object for SSH, S3, GCS, and
        the local filesystem.
        """
        if self._fs is None:
            self._fs = CompositeFilesystem()

            location = self._opts['region'] or _zone_to_region(
                self._opts['zone'])

            self._fs.add_fs('gcs', GCSFilesystem(
                credentials=self._credentials,
                project_id=self._project_id,
                part_size=self._upload_part_size(),
                location=location,
                object_ttl_days=_DEFAULT_CLOUD_TMP_DIR_OBJECT_TTL_DAYS,
            ))

            self._fs.add_fs('local', LocalFilesystem())

        return self._fs

    def _get_tmpdir(self, given_tmpdir):
        """Helper for _fix_tmpdir"""
        if given_tmpdir:
            return given_tmpdir

        # Loop over buckets until we find one that matches region
        # NOTE - because this is a tmpdir, we look for a GCS bucket in the
        # same GCE region
        chosen_bucket_name = None

        # determine region for bucket
        region = self._region()

        for tmp_bucket_name in self.fs.gcs.get_all_bucket_names(
                prefix='mrjob-'):
            tmp_bucket = self.fs.gcs.get_bucket(tmp_bucket_name)

            # NOTE - GCP ambiguous Behavior - Bucket location is being
            # returned as UPPERCASE, ticket filed as of Apr 23, 2016 as docs
            # suggest lowercase. (As of Feb. 12, 2018, this is still true,
            # observed on google-cloud-sdk)
            if tmp_bucket.location.lower() == region:
                # Regions are both specified and match
                log.info("using existing temp bucket %s" % tmp_bucket_name)
                chosen_bucket_name = tmp_bucket_name
                break

        # Example default - "mrjob-us-central1-RANDOMHEX"
        if not chosen_bucket_name:
            chosen_bucket_name = '-'.join(
                ['mrjob', region, random_identifier()])

        return 'gs://%s/tmp/' % chosen_bucket_name

    def _region(self):
        # region of cluster, which is either the region set by the user,
        # or the region derived from the zone they set.
        # used to pick bucket location and name cluster
        return self._opts['region'] or _zone_to_region(self._opts['zone'])

    def _run(self):
        self._launch()
        self._run_steps()

    def _launch(self):
        self._prepare_for_launch()
        self._launch_cluster()

    def _prepare_for_launch(self):
        self._check_output_not_exists()
        self._create_setup_wrapper_scripts()
        self._add_bootstrap_files_for_upload()
        self._add_job_files_for_upload()
        self._upload_local_files()
        self._wait_for_fs_sync()

    def _check_output_not_exists(self):
        """Verify the output path does not already exist. This avoids
        provisioning a cluster only to have Hadoop refuse to launch.
        """
        if self.fs.exists(self._output_dir):
            raise IOError(
                'Output path %s already exists!' % (self._output_dir,))

    def _add_bootstrap_files_for_upload(self):
        """Add files needed by the bootstrap script to self._upload_mgr.

        Create the master bootstrap script if necessary.

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

    def _add_job_files_for_upload(self):
        """Add files needed for running the job (setup and input)
        to self._upload_mgr."""
        if self._opts['hadoop_streaming_jar']:
            self._upload_mgr.add(self._opts['hadoop_streaming_jar'])

        for step in self._get_steps():
            if step.get('jar'):
                self._upload_mgr.add(step['jar'])

    ### Running the job ###

    def cleanup(self, mode=None):
        super(DataprocJobRunner, self).cleanup(mode=mode)

        # close our SSH tunnel, if any
        self._kill_ssh_tunnel()

        # stop the cluster if it belongs to us (it may have stopped on its
        # own already, but that's fine)
        if self._cluster_id and not self._opts['cluster_id']:
            self._cleanup_cluster()

    def _cleanup_cloud_tmp(self):
        # delete all the files we created
        if not self._job_tmpdir:
            return

        try:
            log.info('Removing all files in %s' % self._job_tmpdir)
            self.fs.rm(self._job_tmpdir)
            self._job_tmpdir = None
        except Exception as e:
            log.exception(e)

    # TODO - mtai @ davidmarin - Re-enable log support and supporting cleanup
    def _cleanup_logs(self):
        super(DataprocJobRunner, self)._cleanup_logs()

    def _cleanup_job(self):
        job_prefix = self._dataproc_job_prefix()
        for job in self._list_jobs(
                cluster_name=self._cluster_id,
                state_matcher=_STATE_MATCHER_ACTIVE):
            # Kill all active jobs with the same job_prefix as this job
            job_id = job.reference.job_id

            if not job_id.startswith(job_prefix):
                continue

            self._cancel_job(job_id)
            self._wait_for_api('job cancellation')

    def _cleanup_cluster(self):
        if not self._cluster_id:
            # If we don't have a cluster, then we can't terminate it.
            return

        try:
            log.info("Attempting to terminate cluster")
            self._delete_cluster(self._cluster_id)
        except Exception as e:
            log.exception(e)
            return
        log.info('cluster %s successfully terminated' % self._cluster_id)

    def _wait_for_api(self, msg):
        _wait_for(msg, self._opts['check_cluster_every'])

    def _wait_for_fs_sync(self):
        """Sleep for a little while, to give FS a chance to sync up.
        """
        _wait_for('GCS sync (eventual consistency)',
                  self._opts['cloud_fs_sync_secs'])

    def _streaming_step_job_kwarg(self, step_num):
        """Returns a map from ``'hadoop_job'`` to a dict representing
        a hadoop streaming job.
        """
        return dict(
            hadoop_job=dict(
                args=self._hadoop_streaming_jar_args(step_num),
                main_jar_file_uri=self._hadoop_streaming_jar_uri(),
            )
        )

    def _jar_step_job_kwarg(self, step_num):
        """Returns a map from ``'hadoop_job'`` to a dict representing
        a Hadoop job that runs a JAR"""
        step = self._get_step(step_num)

        hadoop_job = {}

        hadoop_job['args'] = (
            self._interpolate_jar_step_args(step['args'], step_num))

        jar_uri = self._upload_mgr.uri(step['jar'])

        # can't specify main_class and main_jar_file_uri; see
        # https://cloud.google.com/dataproc/docs/reference/rest/v1/projects.regions.jobs#HadoopJob  # noqa
        if step.get('main_class'):
            hadoop_job['jar_file_uris'] = [jar_uri]
            hadoop_job['main_class'] = step['main_class']
        else:
            hadoop_job['main_jar_file_uri'] = jar_uri

        return dict(hadoop_job=hadoop_job)

    def _hadoop_streaming_jar_uri(self):
        if self._opts['hadoop_streaming_jar']:
            return self._upload_mgr.uri(self._opts['hadoop_streaming_jar'])
        else:
            return _HADOOP_STREAMING_JAR_URI

    def _launch_cluster(self):
        """Create an empty cluster on Dataproc, and set self._cluster_id to
        its ID."""
        self.fs.mkdir(self._job_tmpdir)

        # clusterName must be a match of
        # regex '(?:[a-z](?:[-a-z0-9]{0,53}[a-z0-9])?).'
        # as documented in an API error message
        # (not currently documented in the Dataproc docs)
        if not self._cluster_id:
            self._cluster_id = '-'.join(
                ['mrjob', self._region(), random_identifier()])

        # Create the cluster if it's missing, otherwise join an existing one
        try:
            self._get_cluster(self._cluster_id)
            log.info('Adding job to existing cluster - %s' % self._cluster_id)
        except google.api_core.exceptions.NotFound:
            log.info(
                'Creating Dataproc Hadoop cluster - %s' % self._cluster_id)

            cluster_data = self._cluster_create_kwargs()
            self._create_cluster(cluster_data)

            self._wait_for_cluster_ready(self._cluster_id)

        self._set_up_ssh_tunnel()

        # keep track of when we launched our job
        self._dataproc_job_start = time.time()
        return self._cluster_id

    def _wait_for_cluster_ready(self, cluster_id):
        # See https://cloud.google.com/dataproc/reference/rest/v1/projects.regions.clusters#State  # noqa
        cluster_state = None

        # Poll until cluster is ready
        while cluster_state not in ('RUNNING', 'UPDATING'):
            cluster = self._get_cluster(cluster_id)
            cluster_state = cluster.status.State.Name(cluster.status.state)

            if cluster_state in ('ERROR', 'DELETING'):
                raise DataprocException(cluster)

            self._wait_for_api('cluster to accept jobs')

        return cluster_id

    def _dataproc_job_prefix(self):
        return _cleanse_gcp_job_id(self._job_key)

    def _run_steps(self):
        """Wait for every step of the job to complete, one by one."""
        total_steps = self._num_steps()
        # define out steps
        for step_num in range(total_steps):
            job_id = self._launch_step(step_num)

            self._wait_for_step_to_complete(
                job_id, step_num=step_num, num_steps=total_steps)

            log.info('Completed Dataproc Hadoop Job - %s', job_id)

        # After all steps completed, wait for the last output (which is
        # usually written to GCS) to sync
        self._wait_for_fs_sync()

    def _launch_step(self, step_num):
        step = self._get_step(step_num)

        # Clean-up step name
        step_name = '%s---step-%05d-of-%05d' % (
            self._dataproc_job_prefix(), step_num + 1, self._num_steps())

        # Build step

        # job_kwarg is a single-item dict, where the key is 'hadoop_job',
        # 'spark_job', etc.
        if step['type'] == 'streaming':
            job_kwarg = self._streaming_step_job_kwarg(step_num)
        elif step['type'] == 'jar':
            job_kwarg = self._jar_step_job_kwarg(step_num)
        else:
            raise NotImplementedError(
                'Unsupported step type: %r' % step['type'])

        # Submit it
        log.info('Submitting Dataproc Hadoop Job - %s', step_name)
        result = self._submit_job(step_name, job_kwarg)
        log.info('Submitted Dataproc Hadoop Job - %s', step_name)

        job_id = result.reference.job_id
        assert job_id == step_name

        return job_id

    def _wait_for_step_to_complete(self, job_id, step_num, num_steps):
        """Helper for _wait_for_step_to_complete(). Wait for
        step with the given ID to complete, and fetch counters.
        If it fails, attempt to diagnose the error, and raise an
        exception.

        This also adds an item to self._log_interpretations
        """
        log_interpretation = dict(job_id=job_id)
        self._log_interpretations.append(log_interpretation)

        log_interpretation['step'] = {}
        step_type = self._get_step(step_num)['type']

        while True:
            # https://cloud.google.com/dataproc/reference/rest/v1/projects.regions.jobs#JobStatus  # noqa
            job = self._get_job(job_id)

            job_state = job.status.State.Name(job.status.state)

            log.info('%s => %s' % (job_id, job_state))

            log_interpretation['step']['driver_output_uri'] = (
                job.driver_output_resource_uri)

            self._interpret_step_logs(log_interpretation, step_type)

            progress = log_interpretation['step'].get('progress')
            if progress:
                log.info(' ' + progress['message'])

            # https://cloud.google.com/dataproc/reference/rest/v1/projects.regions.jobs#State  # noqa
            # these are the states covered by the ACTIVE job state matcher,
            # plus SETUP_DONE
            if job_state in ('PENDING', 'RUNNING',
                             'CANCEL_PENDING', 'SETUP_DONE'):
                self._wait_for_api('job completion')
                continue

            # print counters if job wasn't CANCELLED
            if job_state != 'CANCELLED':
                self._log_counters(log_interpretation, step_num)

            if job_state == 'ERROR':
                error = self._pick_error(log_interpretation, step_type)
                if error:
                    _log_probable_cause_of_failure(log, error)

            # we're done, will return at the end of this
            if job_state == 'DONE':
                break
            else:
                raise StepFailedException(
                    step_num=step_num, num_steps=num_steps)

    def _default_step_output_dir(self):
        # put intermediate data in HDFS
        return 'hdfs:///tmp/mrjob/%s/step-output' % self._job_key

    ### log intepretation ###

    # step

    def _interpret_step_logs(self, log_interpretation, step_type):
        """Hook for interpreting step logs.

        Unlike with most runners, you may call this multiple times and it
        will continue to parse the step log incrementally, which is useful
        for getting job progress."""
        # don't turn this off even if read_logs opt is false; it's
        # the only way this runner can track job progress

        driver_output_uri = log_interpretation.get(
            'step', {}).get('driver_output_uri')

        if driver_output_uri:
            self._update_step_interpretation(
                log_interpretation['step'], driver_output_uri)

    def _update_step_interpretation(
            self, step_interpretation, driver_output_uri):
        new_lines = self._get_new_driver_output_lines(driver_output_uri)
        _interpret_new_dataproc_step_stderr(step_interpretation, new_lines)

    def _get_new_driver_output_lines(self, driver_output_uri):
        """Get a list of complete job driver output lines that are
        new since the last time we checked.
        """
        state = self._driver_output_state.setdefault(
            driver_output_uri,
            dict(log_uri=None, pos=0, buffer=b''))

        # driver output is in logs with names like driveroutput.000000000
        log_uris = sorted(self.fs.ls(driver_output_uri + '*'))

        for log_uri in log_uris:
            # initialize log_uri with first URI we see
            if state['log_uri'] is None:
                # log the location of job driver output just once
                log.info(
                    '  Parsing job driver output from %s*' % driver_output_uri)
                state['log_uri'] = log_uri

            # skip log files already parsed
            if log_uri < state['log_uri']:
                continue

            # when parsing the next file, reset *pos*
            elif log_uri > state['log_uri']:
                state['pos'] = 0
                state['log_uri'] = log_uri

            log_blob = self.fs.gcs._get_blob(log_uri)

            try:
                new_data = log_blob.download_as_string(start=state['pos'])
            except (google.api_core.exceptions.NotFound,
                    google.api_core.exceptions.RequestRangeNotSatisfiable):
                # blob was just created, or no more data is available
                break

            state['buffer'] += new_data
            state['pos'] += len(new_data)

        # convert buffer into lines, saving leftovers for next time
        stream = BytesIO(state['buffer'])
        state['buffer'] = b''

        lines = []

        for line_bytes in stream:
            if line_bytes.endswith(b'\n'):
                lines.append(to_unicode(line_bytes))
            else:
                # leave final partial line (if any) in buffer
                state['buffer'] = line_bytes

        return lines

    # history

    def _interpret_history_log(self, log_interpretation):
        """Does nothing. We can't get the history logs, and we don't need
        them."""
        if not self._read_logs():
            return

        log_interpretation.setdefault('history', {})

    # task

    def _interpret_task_logs(self, log_interpretation, step_type,
                             error_attempt_ids=(), partial=True):
        """Scan node manager log to find failed container IDs of failed
        tasks, and then scan the corresponding stderr and syslogs."""
        if 'task' in log_interpretation and (
                partial or not log_interpretation['task'].get('partial')):
            return   # already interpreted

        if not self._read_logs():
            return

        step_interpretation = log_interpretation.get('step') or {}

        application_id = step_interpretation.get('application_id')
        if not application_id:
            log.warning(
                "Can't parse node manager logs; missing application ID")
            return

        log_interpretation['task'] = self._task_log_interpretation(
            application_id, step_type, partial)

    def _task_log_interpretation(
            self, application_id, step_type, partial=True):
        """Helper for :py:meth:`_interpret_task_logs`"""
        # not bothering with _read_logs() since this is a helper method
        result = {}

        for container_id in self._failed_task_container_ids(application_id):
            error = _parse_task_syslog_records(
                self._task_syslog_records(
                    application_id, container_id, step_type))

            if not error.get('hadoop_error'):
                # not sure if this ever happens, since we already know
                # which containers failed
                continue

            error['container_id'] = container_id

            # fix weird munging of java stacktrace
            error['hadoop_error']['message'] = _fix_java_stack_trace(
                error['hadoop_error']['message'])

            task_error = _parse_task_stderr(
                self._task_stderr_lines(
                    application_id, container_id, step_type))

            if task_error:
                task_error['message'] = _fix_traceback(task_error['message'])
                error['task_error'] = task_error

            result.setdefault('errors', []).append(error)

            # if partial is true, bail out when we find the first task error
            if task_error and partial:
                result['partial'] = True
                return result

        return result

    def _failed_task_container_ids(self, application_id):
        """Stream container IDs of failed tasks, in reverse order."""
        container_id_prefix = 'container' + application_id[11:]

        log_filter = self._make_log_filter(
            'yarn-yarn-nodemanager',
            {'jsonPayload.class': _CONTAINER_EXECUTOR_CLASS_NAME})

        log.info('Scanning node manager logs for IDs of failed tasks...')

        # it doesn't seem to work to do self.logging_client.logger();
        # there's some RPC dispute about whether the log name should
        # be qualified by project name or not
        entries = self.logging_client.list_entries(
            filter_=log_filter, order_by=google.cloud.logging.DESCENDING)

        for entry in entries:
            message = entry.payload.get('message')
            if not message:
                continue

            m = _CONTAINER_EXIT_RE.match(message)
            if not m:
                continue

            returncode = int(m.group('returncode'))
            if not returncode:
                continue

            container_id = m.group('container_id')
            # matches some other step
            if not container_id.startswith(container_id_prefix):
                continue

            log.debug('  %s' % container_id)
            yield container_id

    def _task_stderr_lines(self, application_id, container_id, step_type):
        """Yield lines from a specific stderr log."""
        log_filter = self._make_log_filter(
            'yarn-userlogs', {
                'jsonPayload.application': application_id,
                'jsonPayload.container': container_id,
                # TODO: pick based on step_type
                'jsonPayload.container_logname': 'stderr',
            })

        log.info('    reading stderr log...')
        entries = self.logging_client.list_entries(filter_=log_filter)

        # use log4j parsing to handle tab -> newline conversion
        for record in _log_entries_to_log4j(entries):
            for line in record['message'].split('\n'):
                yield line

    def _task_syslog_records(self, application_id, container_id, step_type):
        """Yield log4j records from a specific syslog.
        """
        log_filter = self._make_log_filter(
            'yarn-userlogs', {
                'jsonPayload.application': application_id,
                'jsonPayload.container': container_id,
                # TODO: pick based on step_type
                'jsonPayload.container_logname': 'syslog',
            })

        log.info('    reading syslog...')
        entries = self.logging_client.list_entries(filter_=log_filter)

        return _log_entries_to_log4j(entries)

    # misc

    def _make_log_filter(self, log_name=None, extra_values=None):
        # we only want logs from this project, cluster, and region
        d = {}

        d['resource.labels.cluster_name'] = self._cluster_id
        d['resource.labels.project_id'] = self._project_id
        d['resource.labels.region'] = self._region()
        d['resource.type'] = 'cloud_dataproc_cluster'

        if log_name:
            d['logName'] = 'projects/%s/logs/%s' % (
                self._project_id, log_name)

        if extra_values:
            d.update(extra_values)

        return _log_filter_str(d)

    def counters(self):
        return [_pick_counters(log_interpretation)
                for log_interpretation in self._log_interpretations]

    ### Bootstrapping ###

    def get_hadoop_version(self):
        if self._hadoop_version is None:
            self._store_cluster_info()
        return self._hadoop_version

    def get_image_version(self):
        """Get the version that our cluster is running.
        """
        if self._image_version is None:
            self._store_cluster_info()
        return self._image_version

    def _store_cluster_info(self):
        """Set self._image_version and self._hadoop_version."""
        if not self._cluster_id:
            raise ValueError('cluster has not yet been created')

        cluster = self._get_cluster(self._cluster_id)
        self._image_version = (
            cluster.config.software_config.image_version)
        # protect against new versions, including patch versions
        # we didn't explicitly request. See #1428
        self._hadoop_version = map_version(
            self._image_version, _DATAPROC_IMAGE_TO_HADOOP_VERSION)

    def _bootstrap_pre_commands(self):
        # don't run the bootstrap script in / (see #1601)
        return [
            'mkdir /tmp/mrjob',
            'cd /tmp/mrjob',
        ]

    ### Bootstrapping ###

    def _bootstrap_python(self):
        """Return a (possibly empty) list of parsed commands (in the same
        format as returned by parse_setup_cmd())'"""
        if not self._opts['bootstrap_python']:
            return []

        if PY2:
            # Python 2 is already installed; install pip and dev packages
            return [
                ['sudo apt-get install -y python-pip python-dev'],
            ]
        else:
            return [
                ['sudo apt-get install -y python3 python3-pip python3-dev'],
            ]

    def get_cluster_id(self):
        return self._cluster_id

    def _cluster_create_kwargs(self):
        gcs_init_script_uris = []
        if self._master_bootstrap_script_path:
            gcs_init_script_uris.append(
                self._upload_mgr.uri(self._master_bootstrap_script_path))

        cluster_metadata = dict()
        cluster_metadata['mrjob-version'] = mrjob.__version__

        # TODO: remove mrjob-max-secs-idle once lifecycle_config is visible
        # through the gcloud utility and the Google Cloud Console
        cluster_metadata['mrjob-max-secs-idle'] = str(int(
            self._opts['max_mins_idle'] * 60))

        gce_cluster_config = dict(
            metadata=cluster_metadata,
            service_account_scopes=self._opts['service_account_scopes'],
        )

        if self._opts['network']:
            gce_cluster_config['network_uri'] = self._opts['network']

        if self._opts['subnet']:
            gce_cluster_config['subnetwork_uri'] = self._opts['subnet']

        if self._opts['service_account']:
            gce_cluster_config['service_account'] = (
                self._opts['service_account'])

        if self._opts['service_account_scopes']:
            gce_cluster_config['service_account_scopes'] = (
                self._opts['service_account_scopes'])

        if self._opts['zone']:
            gce_cluster_config['zone_uri'] = _gcp_zone_uri(
                project=self._project_id, zone=self._opts['zone'])

        cluster_config = dict(
            gce_cluster_config=gce_cluster_config,
            initialization_actions=[
                dict(executable_file=init_script_uri)
                for init_script_uri in gcs_init_script_uris
            ]
        )

        # Task tracker
        master_conf = _gcp_instance_group_config(
            project=self._project_id, zone=self._opts['zone'],
            count=1, instance_type=self._opts['master_instance_type'],
        )
        if self._opts['master_instance_config']:
            master_conf.update(self._opts['master_instance_config'])

        # Compute + storage
        worker_conf = _gcp_instance_group_config(
            project=self._project_id, zone=self._opts['zone'],
            count=self._opts['num_core_instances'],
            instance_type=self._opts['core_instance_type']
        )
        if self._opts['core_instance_config']:
            worker_conf.update(self._opts['core_instance_config'])

        # Compute ONLY
        secondary_worker_conf = _gcp_instance_group_config(
            project=self._project_id, zone=self._opts['zone'],
            count=self._opts['num_task_instances'],
            instance_type=self._opts['task_instance_type'],
            is_preemptible=True
        )
        if self._opts['task_instance_config']:
            secondary_worker_conf.update(self._opts['task_instance_config'])

        cluster_config['master_config'] = master_conf
        cluster_config['worker_config'] = worker_conf
        if secondary_worker_conf.get('num_instances'):
            cluster_config['secondary_worker_config'] = secondary_worker_conf

        cluster_config['lifecycle_config'] = dict(
            idle_delete_ttl=dict(
                seconds=int(self._opts['max_mins_idle'] * 60)))

        software_config = {}

        if self._opts['cluster_properties']:
            software_config['properties'] = _values_to_text(
                self._opts['cluster_properties'])

        # See - https://cloud.google.com/dataproc/dataproc-versions
        if self._opts['image_version']:
            software_config['image_version'] = self._opts['image_version']

        if software_config:
            cluster_config['software_config'] = software_config

        # in Python 2, dict keys loaded from JSON will be unicode, which
        # the Google protobuf objects don't like
        if PY2:
            cluster_config = _clean_json_dict_keys(cluster_config)

        kwargs = dict(project_id=self._project_id,
                      cluster_name=self._cluster_id,
                      config=cluster_config)

        return self._add_extra_cluster_params(kwargs)

    ### Dataproc-specific Stuff ###

    def _get_cluster(self, cluster_id):
        return self.cluster_client.get_cluster(
            cluster_name=cluster_id,
            **self._project_id_and_region()
        )

    def _create_cluster(self, cluster_data):
        # https://cloud.google.com/dataproc/reference/rest/v1/projects.regions.clusters/create  # noqa
        # https://cloud.google.com/dataproc/reference/rest/v1/projects.regions.clusters/get  # noqa

        self.cluster_client.create_cluster(
            cluster=cluster_data,
            **self._project_id_and_region()
        )

    def _delete_cluster(self, cluster_id):
        return self.cluster_client.delete_cluster(
            cluster_name=cluster_id,
            **self._project_id_and_region()
        )

    def _list_jobs(self, cluster_name=None, state_matcher=None):
        # https://cloud.google.com/dataproc/reference/rest/v1/projects.regions.jobs/list#JobStateMatcher  # noqa
        list_kwargs = self._project_id_and_region()

        if cluster_name:
            list_kwargs['cluster_name'] = cluster_name

        if state_matcher:
            list_kwargs['job_state_matcher'] = state_matcher

        return self.job_client.list_jobs(**list_kwargs)

    def _get_job(self, job_id):
        return self.job_client.get_job(
            job_id=job_id,
            **self._project_id_and_region()
        )

    def _cancel_job(self, job_id):
        return self.job_client.cancel_job(
            job_id=job_id,
            **self._project_id_and_region()
        )

    def _submit_job(self, step_name, job_kwarg):
        # https://cloud.google.com/dataproc/reference/rest/v1/projects.regions.jobs/submit  # noqa
        # https://cloud.google.com/dataproc/reference/rest/v1/projects.regions.jobs#HadoopJob  # noqa
        # https://cloud.google.com/dataproc/reference/rest/v1/projects.regions.jobs#JobReference  # noqa

        submit_job_kwargs = dict(
            job=dict(
                reference=dict(project_id=self._project_id, job_id=step_name),
                placement=dict(cluster_name=self._cluster_id),
                **job_kwarg
            ),
            **self._project_id_and_region()
        )

        log.debug('  submit_job(%s)' % ', '.join(
            '%s=%r' % (k, v) for k, v in sorted(submit_job_kwargs.items())))

        return self.job_client.submit_job(**submit_job_kwargs)

    def _project_id_and_region(self):
        return dict(
            project_id=self._project_id,
            region=(self._opts['region'] or 'global'),
        )

    def _manifest_download_commands(self):
        return [
            # TODO: SSH in and figure out how to use gsutil or similar
            # ('gs://*', 'gsutil cp'),
            ('*://*', 'hadoop fs -copyToLocal'),
        ]

    ### SSH hooks ###

    def _job_tracker_host(self):
        return '%s-m' % self._cluster_id

    def _ssh_tunnel_config(self):
        return _SSH_TUNNEL_CONFIG

    def _launch_ssh_proc(self, args):
        ssh_proc = super(DataprocJobRunner, self)._launch_ssh_proc(args)

        # enter an empty passphrase if creating a key for the first time
        ssh_proc.stdin.write(b'\n\n')

        return ssh_proc

    def _ssh_launch_wait_secs(self):
        """Wait 20 seconds because gcloud has to update project metadata
        (unless we were going to check the cluster sooner anyway)."""
        return min(20.0, self._opts['check_cluster_every'])

    def _ssh_tunnel_args(self, bind_port):
        if not self._cluster_id:
            return

        gcloud_bin = self._opts['gcloud_bin'] or ['gcloud']

        cluster = self._get_cluster(self._cluster_id)
        zone = cluster.config.gce_cluster_config.zone_uri.split('/')[-1]

        return gcloud_bin + [
            'compute', 'ssh',
            '--zone', zone,
            self._job_tracker_host(),
            '--',
        ] + self._ssh_tunnel_opts(bind_port)


def _log_filter_str(name_to_value):
    """return a map from name to value into a log filter query that requires
    each name to equal the given value."""
    return ' AND '.join(
        '%s = %s' % (name, _quote_filter_value(value))
        for name, value in sorted(name_to_value.items()))


def _quote_filter_value(s):
    """Put a string in double quotes, escaping double quote characters"""
    return '"%s"' % s.replace('"', r'\"')


def _log_entries_to_log4j(entries):
    """Convert log entries from a single log file to log4j format, tracking
    line number.

    See :py:meth:`mrjob.logs.log4j._parse_hadoop_log4j_records`
    for format.
    """
    line_num = 0

    for entry in entries:
        message = entry.payload.get('message') or ''

        # NOTE: currently, google.cloud.logging seems strip newlines :(
        num_lines = len(message.split('\n'))

        yield dict(
            caller_location='',
            level=(entry.severity or ''),
            logger=(entry.payload.get('class') or ''),
            message=message,
            num_lines=num_lines,
            start_line=line_num,
            thread='',
            timestamp=(entry.timestamp or ''),
        )

        line_num += num_lines


def _fix_java_stack_trace(s):
    # this is what we get from `gcloud logging`
    if '\n' in s:
        return s
    else:
        return s.replace('\t', '\n\t')


def _fix_traceback(s):
    lines = s.split('\n')

    # strip log4j warnings (which do have proper linebreaks)
    lines = [
        line for line in lines
        if line and not _STDERR_LOG4J_WARNING.match(line)
    ]

    s = '\n'.join(lines)

    if '\n' in s:
        return s  # traceback does have newlines

    s = s.replace('  File', '\n  File')
    s = s.replace('    ', '\n    ')
    s = _TRACEBACK_EXCEPTION_RE.sub(lambda m: '\n' + m.group(0), s)

    return s


def _clean_json_dict_keys(x):
    """Cast any dictionary keys in the given JSON object to str.
    We can assume that x isn't a recursive data structure, and that
    this is only called in Python 2."""
    if isinstance(x, dict):
        return {str(k): _clean_json_dict_keys(v) for k, v in x.items()}
    elif isinstance(x, list):
        return [_clean_json_dict_keys(item) for item in x]
    else:
        return x


def _values_to_text(d):
    """Return a dictionary with the same keys as *d*, but where the
    non-string, non-bytes values have been JSON-encoded.

    Used to encode cluster properties.
    """
    result = {}

    for k, v in d.items():
        if not isinstance(v, (string_types, bytes)):
            v = json.dumps(v)

        result[k] = v

    return result


def _fully_qualify_scope_uri(uri):
    if is_uri(uri):
        return uri
    else:
        return 'https://www.googleapis.com/auth/%s' % uri
