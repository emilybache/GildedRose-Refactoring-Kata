# TextTest regression tests

This folder contains Text-Based tests for the GildedRose Refactoring Kata.

These tests are designed to be used with the open source testing tool "TextTest", available from http://texttest.org You can run them without it too though, see below.

## Running without TextTest

This should be perfectly possible, but is probably less convenient than using TextTest. 

Write a script that will execute the SUT (see "config.gr" for details of the executables), giving the commandline options listed in "options.gr". Collect the output from standard output in a file, and diff that against the golden copy "stdout.gr". Any diff is a test failure.

## Running with TextTest

- Install TextTest (see http://texttest.org)
- set $TEXTTEST_HOME environment variable to point at the "texttests" folder
- run texttest using a command like "python texttest.py -a gr"

This should start the GUI for the TextTest tool.

Each test case has it's own subdirectory. The name of the directory is the name of the test - in this case "ThirtyDays". The "Golden Copy" of the output for that test case is kept in that directory. In this case we have three files:

- __stderr.gr__ - the expected output to Standard Error (stderr)
- __stdout.gr__ - the expected output to Standard Output (stdout)
- __options.gr__ - the options to give on the command line when you run the System Under Test (SUT)

In the directory above, there are configuration files for TextTest:

- __config.gr__ - this tells TextTest where to find the SUT executable, and sets up options for how it runs the SUT and interprets the output.
- __environment.gr__ - this file lists environment variables that will be set before TextTest runs the SUT. This is especially important for Java applications, that must set the CLASSPATH environment variable in order to run properly.
- __testsuite.gr__ - lists the constituent test cases of this suite. Change the order of the entries here to change the order they appear in the TextTest GUI.

To run a test, click on it in the GUI and select "Run". TextTest will run it in a temporary (sandbox) directory and report the results. If the test fails, you can double click on a file to see the diff against the Golden Copy.

If you run into difficulties with TextTest, there is documentation available on [texttest.org](http://texttest.org), or you can ask a question on the [mailing list](https://lists.sourceforge.net/lists/listinfo/texttest-users).


