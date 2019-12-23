#!/usr/bin/env bash

set -ex

echo "Create database"
psql -d postgres -c 'DROP DATABASE IF EXISTS kata;'
psql -d postgres -c 'CREATE DATABASE kata;'
psql -d kata -c 'CREATE EXTENSION DBLINK;'

echo "Initialize test framework"
wget https://raw.githubusercontent.com/adrianandrei-ca/pgunit/bc69dfc526ec3db55ff72af5d78eab55661502af/PGUnit.sql \
 && psql -d kata -f PGUnit.sql \
 && rm PGUnit.sql

echo "Initialize custom asserts"
psql -d kata -f asserts.sql

echo "Add current code"
psql -d kata -f item.sql
psql -d kata -f new_item.sql
psql -d kata -f update_quality.sql
