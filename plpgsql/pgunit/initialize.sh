#!/usr/bin/env bash

set -ex

echo "Enable DBLINK"
psql -d kata -c 'CREATE EXTENSION DBLINK;'

echo "Initialize test framework"
wget https://raw.githubusercontent.com/adrianandrei-ca/pgunit/bc69dfc526ec3db55ff72af5d78eab55661502af/PGUnit.sql \
 && psql -d kata -f PGUnit.sql \
 && rm PGUnit.sql

echo "Initialize custom asserts"
psql -d kata -f asserts.sql
