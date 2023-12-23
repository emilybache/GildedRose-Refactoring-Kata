#!/bin/bash

./texttest_fixture.sh 30 | diff -u - ../texttests/ThirtyDays/stdout.gr
