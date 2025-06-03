#!/bin/sh

if [ ! -d "venv" ]; then
    python -m venv venv
fi
venv/Scripts/pip.exe install texttest
venv/Scripts/texttest.exe -d . -con "$@"
