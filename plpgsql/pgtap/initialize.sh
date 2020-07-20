#!/usr/bin/env bash

set -ex

echo "Enable extension"
psql -d kata -c 'CREATE EXTENSION IF NOT EXISTS pgtap;'
