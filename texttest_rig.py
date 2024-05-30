#!/usr/bin/env python
"""
This script uses Gradle to execute the TexttestFixture.
It is designed to be used by TextTest and specified in the file 'texttests/config.gr' in this repo.
It is more convenient for TextTest to use since Gradle needs
several arguments in addition to the one the TextTest fixture needs.
"""
import os
import subprocess
import sys

args = " ".join(sys.argv[1:])
TEXTTEST_HOME = os.environ.get("TEXTTEST_HOME", os.getcwd())
subprocess.run(f"{TEXTTEST_HOME}/Java/gradlew -p {TEXTTEST_HOME}/Java -q text --args {args}", shell=True)
