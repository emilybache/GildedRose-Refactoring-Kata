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



#args = " ".join(sys.argv[1:])
#TEXTTEST_HOME = os.environ.get("TEXTTEST_HOME", os.getcwd())
#subprocess.run(f"{TEXTTEST_HOME}/Kotlin/gradlew -p {TEXTTEST_HOME}/Kotlin -q run --args {args}", shell=True)



# MINE / UPDATED
TEXTTEST_HOME = r"C:\Users\pedan\IdeaProjects\GildedRose-Refactoring-Kata\GildedRose-Refactoring-Kata"
gradlew_path = os.path.join(TEXTTEST_HOME, "Kotlin", "gradlew")
subprocess.run(f'"{gradlew_path}" -p "{TEXTTEST_HOME}\\Kotlin" -q text', shell=True)