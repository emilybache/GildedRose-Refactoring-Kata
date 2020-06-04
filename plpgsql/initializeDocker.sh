#!/usr/bin/env bash

wait_database()
{
    while :
    do
        (echo > /dev/tcp/127.0.0.1/5432) >/dev/null 2>&1
        result=$?

        if [[ $result -eq 0 ]]; then
            break
        fi
        sleep 1
    done
    return $result
}

nohup docker-entrypoint.sh postgres  > /dev/null 2>&1 &
wait_database

set -ex

./initializeDatabase.sh

echo "Stop database"
disown %1
