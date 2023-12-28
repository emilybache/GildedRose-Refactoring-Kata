#!/bin/bash

GILDED_ROSE_SCRIPT="./gilded_rose.sh"

get_name() {
    grep -o '^[^|]*'
}

assert_equals() {
    local expected="$1"
    diff -u - <(echo "$expected")
}

test_foo() {
    echo "foo|0|0" |
        bash "$GILDED_ROSE_SCRIPT" |
        get_name |
        assert_equals "fixme"
}

test_foo
