# -*- coding: utf-8 -*-
# Copyright 2018 Yelp
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
"""Utilities for creating custom AMIs."""
import logging
import re

log = logging.getLogger(__name__)

# used to match the name of AMIs that we want to use on EMR
_EMR_BASE_AMI_NAME_RE = re.compile(r'^amzn-ami-hvm-[\d\.]*-x86_64-ebs$')


def describe_base_emr_images(ec2_client):
    """Fetch a list of Amazon Linux AMI images that are usable with EMR,
    with the most recent first. This can take several seconds.

    :param ec2_client: a boto3 EC2 client, which can be obtained from
                       :py:meth:`mrjob.emr.EMRJobRunner.make_ec2_client()`
                       or ``boto3.client('ec2')``

    For the sake of consistency, we have somewhat stricter requirements
    than `the AWS documentation <https://docs.aws.amazon.com/emr/latest/\
    ManagementGuide/emr-custom-ami.html#emr-custom-ami-considerations>`_.
    Specifically:

    * Amazon Linux (not Amazon Linux 2)
    * HVM virtualization
    * x86_64 architecture
    * single EBS volume
      * standard volume type (not GP2)
    * stable version (no "testing" or "rc", only numbers and dots)

    This only returns images going back to September 2016 (prior to that,
    EC2 used a different naming convention).

    This returns a dictionary for each image, in the same response format as
    `ec2_client.describe_images() <https://boto3.amazonaws.com/v1/\
    documentation/api/latest/reference/services/ec2.html#EC2.Client\
    .describe_images>`_. The
    *ImageId* field contains the AMI ID, and *Description* contains
    a human-readable description.
    """
    # DescribeImages' filtering is imperfect and slow, but this helps a bit
    images = ec2_client.describe_images(
        Owners=['amazon'],
        Filters=[
            dict(Name='architecture', Values=['x86_64']),
            dict(Name='root-device-type', Values=['ebs']),
            dict(Name='virtualization-type', Values=['hvm']),
        ],
    )['Images']

    # perform further filtering by name to pick out Amazon Linux
    images = [img for img in images
              if _EMR_BASE_AMI_NAME_RE.match(img.get('Name') or '')]

    # filter out any images that have multiple volumes
    # (this is implied by the naming convention, but just in case)
    images = [img for img in images
              if len(img.get('BlockDeviceMappings') or []) == 1]

    # require a CreationDate
    images = [img for img in images if img['CreationDate']]

    # put most recent images first
    images.sort(key=lambda img: img['CreationDate'], reverse=True)

    return images
