#!/bin/bash

./texttest_fixture.sh 30 |
    diff -u - ../texttests/ThirtyDays/stdout.gr &&
    echo "✅ looks good" ||
    (echo "❌ failed" && exit 1)
