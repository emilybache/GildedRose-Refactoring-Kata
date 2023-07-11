# TextTest regression tests

This folder contains Text-Based Approval tests for the GildedRose Refactoring Kata. They are fairly comprehensive and well worth using if you'd prefer to go straight to the refactoring without writing your own tests first.

These tests are designed to be used with the open source testing tool "TextTest", available from [http://texttest.org](http://texttest.org).

## Install TextTest 

There are install instructions on the [texttests website](http://texttest.sourceforge.net/index.php?page=documentation_4_0&n=install_texttest). If you are happy to run without the Graphical User Interface, then you only need python3 and pip:

	> pip install texttest

## Configure language version

Before you can run the tests you need to tell texttest which language version of GildedRose you plan to refactor. Open the file 'config.gr' and edit it. Several languages are supported. All lines starting with '#' are comments in this file. Find the lines referring to the language you want, and uncomment them. (Note some languages like Java need several lines uncommented)

While you're here, change the settings for editor and diff program to match your preferences. By default it uses 'subl' and 'meld'. It will accept any editors or diff programs that you can run from the command line.

## running TextTest

Start texttest from the folder above the one this file is in. Texttest detects the current working directory and uses that as the variable $TEXTTEST_HOME in the config.gr file.

    # replace this path with wherever you cloned this repo
    > cd /home/ec2-user/workspace/GildedRose-Refactoring-Kata 
	> texttest &

This should start the GUI for the TextTest tool. Select the test case "ThirtyDays" and press the "Run" button. This will open a new 'runner' window for each test run.

If the texttest GUI doesn't work, or you prefer to use the command line, use this instead:

	> texttest -con

That will run all the test cases it finds and report the results.

## Running without TextTest

This should be perfectly possible, but is probably less convenient than using TextTest. 

Write a script that will execute the system under test (see "config.gr" for details of the executables), giving the commandline options listed in "options.gr". Collect the output from standard output in a file, and diff that against the golden copy "stdout.gr". Any diff is a test failure.

## Explaining TextTest test cases

Each test case has it's own subdirectory. The name of the directory is the name of the test - in this case "ThirtyDays". The "Golden Master" of the output for that test case is kept in that directory. In this case we have three files:

- __stderr.gr__ - the expected output to Standard Error (stderr)
- __stdout.gr__ - the expected output to Standard Output (stdout)
- __options.gr__ - the options to give on the command line when you run the System Under Test (SUT)

In the directory above, there are configuration files for TextTest:

- __config.gr__ - this tells TextTest where to find the SUT executable, and sets up options for how it runs the SUT and interprets the output.
- __environment.gr__ - this file lists environment variables that will be set before TextTest runs the SUT. This is especially important for Java applications, that must set the CLASSPATH environment variable in order to run properly.
- __testsuite.gr__ - lists the constituent test cases of this suite. Change the order of the entries here to change the order they appear in the TextTest GUI.

To run a test, click on it in the GUI and select "Run". TextTest will run it in a temporary (sandbox) directory and report the results. If the test fails, you can double click on a file to see the diff against the Golden Copy.

If you run into difficulties with TextTest, there is documentation available on [texttest.org](http://texttest.org), or you can ask a question on the [mailing list](https://lists.sourceforge.net/lists/listinfo/texttest-users).

## Introduction to Text-Based Approval Testing

This is a testing approach which is very useful when refactoring legacy code. Before you change the code, you run it, and gather the output of the code as a plain text file. You review the text, and if it correctly describes the behaviour as you understand it, you can "approve" it, and save it as a "Golden Master". Then after you change the code, you run it again, and compare the new output against the Golden Master. Any differences, and the test fails.

It's basically the same idea as "assertEquals(expected, actual)" in a unit test, except the text you are comparing is typically much longer, and the "expected" value is saved from actual output, rather than being defined in advance.

Typically a piece of legacy code may not produce suitable textual output from the start, so you may need to modify it before you can write your first text-based approval test. One way to do that is to write a "main" method that executes the code and prints out what the result is afterwards. Each language version has implemented a texttest 'fixture' that does this. It runs the GildedRose 'update_quality' method once each day for 30 days, printing the item state each day.

