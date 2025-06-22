#!/bin/sh

# Copyright 2013 Lyft
# Copyright 2014 Alex Konradi
# Copyright 2015 Yelp and Contributors
# Copyright 2016-2017 Yelp
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

# Author: David Marin <dm@davidmarin.org>

# This script is part of mrjob, but can be run as a bootstrap action on
# ANY Elastic MapReduce cluster. Arguments are totally optional.

# This script runs `hadoop job -list` in a loop and considers the cluster
# idle if no jobs are currently running. If the cluster stays idle long
# enough AND we're close enough to the end of an EC2 billing hour, we
# shut down the master node, which kills the cluster.

# By default, we allow an idle time of 5 minutes. Also, be default, we
# won't terminate the cluster in the first 10 minutes this script runs.

# Caveats:

# Race conditions: this script can only see currently running jobs, not ones
# pending in EMR, or ones that you're about to submit, or jobs that started
# running since the last time we called `hadoop job -list`.

# This script will leave the cluster in the TERMINATED_WITH_ERRORS state,
# with LastStateChangeReason "The master node was terminated. ". It can
# take EMR a minute or so to realize that master node has been shut down.

# full usage:
#
# ./terminate_idle_cluster_emr.sh [max_secs_idle [grace_period [log_path]]]
#
# Both arguments must be integers

set -x  # enable echo

MAX_SECS_IDLE=$1
if [ -z "$MAX_SECS_IDLE" ]; then MAX_SECS_IDLE=300; fi

GRACE_PERIOD_SECS=$2
if [ -z "$GRACE_PERIOD_SECS" ]; then GRACE_PERIOD_SECS=600; fi

LOG_PATH=$3
if [ -z "$LOG_PATH" ]
then
    LOG_PATH=/mnt/var/log/bootstrap-actions/mrjob-idle-termination.log
fi

# exit if this isn't the master node
grep -q 'isMaster.*false' /mnt/var/lib/info/instance.json && exit 0

(
while true  # the only way out is to SHUT DOWN THE MACHINE
do
    # get the uptime as an integer (expr can't handle decimals)
    UPTIME=$(cat /proc/uptime | cut -f 1 -d .)

    if [ -z "$START" ]
    then
        START=$UPTIME
    fi

    # if LAST_ACTIVE hasn't been initialized, hadoop hasn't been installed
    # yet (this happens on 4.x AMIs), or there are jobs running, just set
    # LAST_ACTIVE to UPTIME. This also checks yarn application if it
    # exists (see #1145)
    if [ -z "$LAST_ACTIVE" ] || \
        ! which hadoop > /dev/null || \
        nice hadoop job -list 2> /dev/null | grep -q '^\s*job_' || \
        (which yarn > /dev/null && \
            nice yarn application -list 2> /dev/null | \
            grep -v 'Total number' | grep -q RUNNING)
    then
        LAST_ACTIVE=$UPTIME
    else
        SECS_RUN=$(expr $UPTIME - $START)
        if expr $SECS_RUN '>' $GRACE_PERIOD_SECS > /dev/null
        then

            # the cluster is idle! how long has this been going on?
            SECS_IDLE=$(expr $UPTIME - $LAST_ACTIVE)

            if expr $SECS_IDLE '>' $MAX_SECS_IDLE > /dev/null
            then
                sudo shutdown -h now
                # continue looping in case something went wrong (see #1819)
            fi
        fi
    fi

    # sleep so we don't peg the CPU
    sleep 5
done
# close stdin to daemonize the script; otherwise bootstrapping
# never finishes
) 0<&- &> $LOG_PATH &
