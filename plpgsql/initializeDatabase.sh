#!/usr/bin/env bash

set -ex

echo "Create database"
psql -d postgres -c 'DROP DATABASE IF EXISTS kata;'
psql -d postgres -c 'CREATE DATABASE kata;'

./initialize.sh

echo "Add current code"
psql -d kata -f src/item.sql
psql -d kata -f src/new_item.sql
