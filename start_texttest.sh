#!/bin/sh

if [ ! -d "venv" ]; then
    python -m venv venv
fi
venv/bin/pip install texttest
venv/bin/texttest -d . -con "$@"
