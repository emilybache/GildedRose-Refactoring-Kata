# -*- coding: utf-8 -*-
# Copyright 2013 Lyft
# Copyright 2015-2016 Yelp
# Copyright 2017 Yelp and Contributors
# Copyright 2019 Yelp and Contributors
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
"""General information about Amazon Web Services, such as region-to-endpoint
mappings. Also includes basic utilities for working with boto3.
"""
import socket
import time
from datetime import datetime

# dateutil is a boto3 dependency
try:
    from dateutil.tz import tzutc
    tzutc
except ImportError:
    tzutc = None

try:
    import boto3
    boto3  # quiet "redefinition of unused ..." warning from pyflakes
except ImportError:
    boto3 = None

try:
    import botocore.client
    botocore  # quiet "redefinition of unused ..." warning from pyflakes
except ImportError:
    botocore = None

# ssl is a built-in package, but isn't available if no SSL library is installed
try:
    from ssl import SSLError
except ImportError:
    SSLError = None

from mrjob.retry import RetryWrapper


### EC2 Instances ###

# map from instance type to number of compute units, from:
# - https://aws.amazon.com/ec2/pricing/
# - http://aws.amazon.com/ec2/previous-generation/
EC2_INSTANCE_TYPE_TO_COMPUTE_UNITS = {
    'c1.medium': 5,
    'c1.xlarge': 20,
    'c3.2xlarge': 28,
    'c3.4xlarge': 55,
    'c3.8xlarge': 108,
    'c3.large': 7,
    'c3.xlarge': 14,
    'c4.2xlarge': 31,
    'c4.4xlarge': 62,
    'c4.8xlarge': 132,
    'c4.large': 8,
    'c4.xlarge': 16,
    'c5.18xlarge': 281,
    'c5.2xlarge': 34,
    'c5.4xlarge': 68,
    'c5.9xlarge': 141,
    'c5.large': 9,
    'c5.xlarge': 17,
    'cc1.4xlarge': 33.5,
    'cc2.8xlarge': 88,
    'cg1.4xlarge': 33.5,
    'cr1.8xlarge': 88,
    'd2.2xlarge': 27,
    'd2.4xlarge': 56,
    'd2.8xlarge': 116,
    'd2.xlarge': 14,
    'g2.2xlarge': 26,
    'g2.8xlarge': 104,
    'hi1.4xlarge': 35,
    'hs1.8xlarge': 35,
    'i2.2xlarge': 27,
    'i2.4xlarge': 53,
    'i2.8xlarge': 104,
    'i2.xlarge': 14,
    'm1.large': 4,
    'm1.medium': 2,
    'm1.small': 1,
    'm1.xlarge': 8,
    'm2.2xlarge': 13,
    'm2.4xlarge': 26,
    'm2.xlarge': 6.5,
    'm3.2xlarge': 26,
    'm3.large': 6.5,
    'm3.medium': 3,
    'm3.xlarge': 13,
    'm4.10xlarge': 124.5,
    'm4.16xlarge': 188,
    'm4.2xlarge': 26,
    'm4.4xlarge': 53.5,
    'm4.large': 6.5,
    'm4.xlarge': 13,
    'm5.12xlarge': 173,
    'm5.24xlarge': 345,
    'm5.2xlarge': 31,
    'm5.4xlarge': 60,
    'm5.large': 8,
    'm5.xlarge': 16,
    'r3.2xlarge': 26,
    'r3.4xlarge': 52,
    'r3.8xlarge': 104,
    'r3.large': 6.5,
    'r3.xlarge': 13,
    'r4.16xlarge': 195,
    'r4.2xlarge': 27,
    'r4.4xlarge': 53,
    'r4.8xlarge': 99,
    'r4.large': 7,
    'r4.xlarge': 13.6,
}

# map from instance type to GB of memory
# from http://aws.amazon.com/ec2/instance-types/
# and http://aws.amazon.com/ec2/previous-generation/
EC2_INSTANCE_TYPE_TO_MEMORY = {
    'c1.medium': 1.7,
    'c1.xlarge': 7,
    'c3.2xlarge': 15,
    'c3.4xlarge': 30,
    'c3.8xlarge': 60,
    'c3.large': 3.75,
    'c3.xlarge': 7.5,
    'c4.2xlarge': 15,
    'c4.4xlarge': 30,
    'c4.8xlarge': 60,
    'c4.large': 3.75,
    'c4.xlarge': 7.5,
    'c5.18xlarge': 144,
    'c5.2xlarge': 16,
    'c5.4xlarge': 32,
    'c5.9xlarge': 72,
    'c5.large': 4,
    'c5.xlarge': 8,
    'cc1.4xlarge': 23,
    'cc2.8xlarge': 60.5,
    'cg1.4xlarge': 22.5,
    'cr1.8xlarge': 244,
    'd2.2xlarge': 61,
    'd2.4xlarge': 122,
    'd2.8xlarge': 244,
    'd2.xlarge': 30.5,
    'g2.2xlarge': 15,
    'g2.8xlarge': 60,
    'hi1.4xlarge': 60.5,
    'hs1.8xlarge': 117,
    'i2.2xlarge': 61,
    'i2.4xlarge': 122,
    'i2.8xlarge': 244,
    'i2.xlarge': 30.5,
    'm1.large': 7.5,
    'm1.medium': 3.75,
    'm1.small': 1.7,
    'm1.xlarge': 17.1,
    'm2.2xlarge': 34.2,
    'm2.4xlarge': 68.4,
    'm2.xlarge': 17.5,
    'm3.2xlarge': 30,
    'm3.large': 7.5,
    'm3.medium': 3.75,
    'm3.xlarge': 15,
    'm4.10xlarge': 160,
    'm4.16xlarge': 256,
    'm4.2xlarge': 32,
    'm4.4xlarge': 64,
    'm4.large': 8,
    'm4.xlarge': 16,
    'm5.12xlarge': 192,
    'm5.24xlarge': 384,
    'm5.2xlarge': 32,
    'm5.4xlarge': 64,
    'm5.large': 8,
    'm5.xlarge': 16,
    'r3.2xlarge': 61,
    'r3.4xlarge': 122,
    'r3.8xlarge': 244,
    'r3.large': 15,
    'r3.xlarge': 30.5,
    'r4.16xlarge': 488,
    'r4.2xlarge': 61,
    'r4.4xlarge': 122,
    'r4.8xlarge': 244,
    'r4.large': 15.25,
    'r4.xlarge': 30.5,
}


### Regions ###

# us-east-1 doesn't have its own endpoint or need bucket location constraints
_S3_REGION_WITH_NO_LOCATION_CONSTRAINT = 'us-east-1'

# The region to assume if none is specified
_DEFAULT_AWS_REGION = 'us-east-1'


### Utilities ###

# if AWS throttles us, how long to wait (in seconds) before trying again?
_AWS_BACKOFF = 20
_AWS_BACKOFF_MULTIPLIER = 1.5
_AWS_MAX_TRIES = 20  # this takes about a day before we run out of tries


def _client_error_code(ex):
    """Get the error code for the given ClientError"""
    return ex.response.get('Error', {}).get('Code', '')


def _client_error_status(ex):
    """Get the HTTP status for the given ClientError"""
    resp = ex.response
    # sometimes status code is in ResponseMetadata, not Error
    return (resp.get('Error', {}).get('HTTPStatusCode') or
            resp.get('ResponseMetadata', {}).get('HTTPStatusCode'))


def _is_retriable_client_error(ex):
    """Is the exception from a boto3 client retriable?"""
    if isinstance(ex, botocore.exceptions.ClientError):
        # these rarely get through in boto3
        code = _client_error_code(ex)
        # "Throttl" catches "Throttled" and "Throttling"
        if any(c in code for c in ('Throttl', 'RequestExpired', 'Timeout')):
            return True
        # spurious 505s thought to be part of an AWS load balancer issue
        return _client_error_status(ex) == 505
    # in Python 2.7, SSLError is a subclass of socket.error, so catch
    # SSLError first
    elif isinstance(ex, SSLError):
        # catch ssl.SSLError: ('The read operation timed out',). See #1827.
        # also catches 'The write operation timed out'
        return any(isinstance(arg, str) and 'timed out' in arg
                   for arg in ex.args)
    elif isinstance(ex, socket.error):
        return ex.args in ((104, 'Connection reset by peer'),
                           (110, 'Connection timed out'))
    else:
        return False


def _wrap_aws_client(raw_client, min_backoff=None):
    """Wrap a given boto3 Client object so that it can retry when
    throttled."""
    return RetryWrapper(
        raw_client,
        retry_if=_is_retriable_client_error,
        backoff=max(_AWS_BACKOFF, min_backoff or 0),
        multiplier=_AWS_BACKOFF_MULTIPLIER,
        max_tries=_AWS_MAX_TRIES,
        unwrap_methods={'get_paginator'})


def _boto3_now():
    """Get a ``datetime`` that's compatible with :py:mod:`boto3`.
    These are always UTC time, with time zone ``dateutil.tz.tzutc()``.
    """
    if tzutc is None:
        raise ImportError(
            'You must install dateutil to get boto3-compatible datetimes')

    return datetime.now(tzutc())


def _boto3_paginate(what, boto3_client, api_call, **api_params):
    """Yield results from a paginatable API client call.

    *what* is the name of the field the holds the list of items
    in each page (e.g. ``'InstanceGroups'``).

    This doesn't do anything magical; it just saves the trouble of creating
    variable names for your paginator and its pages.

    You can add the keyword ``_delay`` to *api_params* to sleep that many
    seconds after each API call
    """
    # added the _delay kwarg for the audit-emr-usage tool; see #1091
    _delay = api_params.pop('_delay', None)

    paginator = boto3_client.get_paginator(api_call)

    for page in paginator.paginate(**api_params):
        if _delay:
            time.sleep(_delay)

        for item in page[what]:
            yield item
