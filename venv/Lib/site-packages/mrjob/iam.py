# -*- coding: utf-8 -*-
# Copyright 2015-2016 Yelp
# Copyright 2017 Yelp
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
"""Utilities for dealing with AWS IAM.

The main purpose of this code is to create the needed IAM instance profiles,
roles, and policies to make EMR work with a new account.

This works somewhat similarly to the AWS CLI (aws emr create-default-roles),
except they are assigned random mrjob-* names.

There isn't really versioning; mrjob will simply check if there are IAM objects
idential to the ones it needs before attempting to create them.
"""
import json
from logging import getLogger

from mrjob.aws import _boto3_paginate
from mrjob.util import random_identifier

# Working IAM roles and policies for EMR; these should be identical
# to the ones created by AWS CLI (`aws emr create-default-roles`), at least as
# of 2015-04-20.
#
# AWS has recommended roles in their documentation, but they don't quite
# work as-is:
#
# http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-iam-roles-defaultroles.html  # noqa

# use this for service_role
_MRJOB_SERVICE_ROLE = {
    "Version": "2008-10-17",
    "Statement": [{
        "Sid": "",
        "Effect": "Allow",
        "Principal": {
            "Service": "elasticmapreduce.amazonaws.com"
        },
        "Action": "sts:AssumeRole"
    }]
}

# Role to wrap in an instance profile
_MRJOB_INSTANCE_PROFILE_ROLE = {
    "Version": "2008-10-17",
    "Statement": [{
        "Sid": "",
        "Effect": "Allow",
        "Principal": {
            "Service": "ec2.amazonaws.com"
        },
        "Action": "sts:AssumeRole"
    }]
}

# the built-in, managed policy to attach to _MRJOB_SERVICE_ROLE
_EMR_SERVICE_ROLE_POLICY_ARN = (
    'arn:aws:iam::aws:policy/service-role/AmazonElasticMapReduceRole')

# the built-in, managed policy to attach to _MRJOB_INSTANCE_PROFILE_ROLE
_EMR_INSTANCE_PROFILE_POLICY_ARN = (
    'arn:aws:iam::aws:policy/service-role/AmazonElasticMapReduceforEC2Role')

# if we can't create or find our own service role, use the one
# created by the AWS console and CLI
_FALLBACK_SERVICE_ROLE = 'EMR_DefaultRole'

# if we can't create or find our own instance profile, use the one
# created by the AWS console and CLI
_FALLBACK_INSTANCE_PROFILE = 'EMR_EC2_DefaultRole'


log = getLogger(__name__)


# Auto-created roles/profiles

def get_or_create_mrjob_service_role(client):
    """Look for a usable service role for EMR, and if there is none,
    create one. Either way, return that role's name."""

    # look for matching role. Must have same policy document
    # and attached role policy
    for role in _boto3_paginate('Roles', client, 'list_roles'):
        if _role_matches(client, role, _MRJOB_SERVICE_ROLE,
                         _EMR_SERVICE_ROLE_POLICY_ARN):
            return role['RoleName']

    # no matches, create it ourselves
    role_name = _create_mrjob_role_with_attached_policy(
        client, _MRJOB_SERVICE_ROLE, _EMR_SERVICE_ROLE_POLICY_ARN)

    log.info('Auto-created service role %s' % role_name)

    return role_name


def get_or_create_mrjob_instance_profile(client):
    """Look for a usable instance profile for EMR, and if there is none,
    create one."""
    # look for matching instance profile. Must point to a role with
    # the right policy document and attached role policy
    for profile in _boto3_paginate(
            'InstanceProfiles', client, 'list_instance_profiles'):
        roles = profile['Roles']
        if len(roles) != 1:
            continue
        if _role_matches(client, roles[0], _MRJOB_INSTANCE_PROFILE_ROLE,
                         _EMR_INSTANCE_PROFILE_POLICY_ARN):
            return profile['InstanceProfileName']

    # create a new role, and wrap it in an instance profile
    # with the same name
    name = _create_mrjob_role_with_attached_policy(
        client, _MRJOB_INSTANCE_PROFILE_ROLE, _EMR_INSTANCE_PROFILE_POLICY_ARN)

    client.create_instance_profile(InstanceProfileName=name)
    client.add_role_to_instance_profile(InstanceProfileName=name,
                                        RoleName=name)

    log.info('Auto-created instance profile %s' % name)

    return name


def _role_matches(client, role, role_document, policy_arn):
    """Does the given role data structure have the given policy document
    and the given policy ARN attached?

    (Roles can have up to two policy ARNs attached, but we don't need this
    functionality.)
    """
    if role['AssumeRolePolicyDocument'] != role_document:
        return False

    # not bothering to paginate these because we're only checking for
    # a single one. As of 2015-05-29, the max allowed was two anyway.
    policy_resp = client.list_attached_role_policies(RoleName=role['RoleName'])
    policies = policy_resp['AttachedPolicies']
    return len(policies) == 1 and policies[0]['PolicyArn'] == policy_arn


def _create_mrjob_role_with_attached_policy(client, role_document, policy_arn):
    """Create a new role with a random name starting with ``mrjob-`` that
    has the given policy document and the given policy ARN attached.

    (Roles can have up to two policy ARNs attached, but we don't need this
    functionality.)
    """
    # create role
    role_name = 'mrjob-' + random_identifier()

    client.create_role(AssumeRolePolicyDocument=json.dumps(role_document),
                       RoleName=role_name)
    client.attach_role_policy(PolicyArn=policy_arn,
                              RoleName=role_name)

    return role_name
