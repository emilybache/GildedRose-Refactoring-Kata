# TextTest regression tests

This folder contains Text-Based Approval tests for the GildedRose Refactoring Kata designed by Emily Bache. They are fairly comprehensive and well worth using if you'd prefer to go straight to the refactoring without writing your own tests first.

These tests are designed to be used with the open source testing tool "TextTest", available from [http://texttest.org](http://texttest.org).


## Configure the language version you want to test

Before you can run the tests you need to tell texttest which language version of GildedRose you plan to refactor. Open the file 'config.gr' and edit it. Several languages are supported. All lines starting with '#' are comments in this file. Find the lines referring to the language you want, and uncomment them. 

While you're here, change the settings for editor and diff program to match your preferences. By default it uses 'subl' and 'meld'. It will accept any editors or diff programs that you can run from the command line.

## Running TextTest

The instructions are slightly different depending on your platform.

### Running TextTest on Linux or MacOS

There is a convenience script 'start_texttest.sh' in the root folder of this repo. This script assumes you already have Python installed. This script will first create a virtual python environment and install texttest if you haven't done that before, then run the tests.

### Running TextTest on Windows

Download the installer as explained on [TextTest.org](http://www.texttest.org/getting_started/install_windows.html). Make sure the texttest executable is on your PATH. Then use the convenience script 'start_texttest.bat'  in the root folder of this repo to run the tests on the console.

Windows may warn you that it doesn't trust this installer and be reluctant to download it. If you prefer not to continue with this, an alternative is to run TextTest via Python. First install Python then use the convenience script 'start_texttest_from_python.bat'.

## Interpreting Test Results

You should see output like this if the test passes:

    Using local queues for Application Gilded Rose Refactoring Kata
    Q: Submitting Gilded Rose Refactoring Kata test-case ThirtyDays to default local queue
    S: Gilded Rose Refactoring Kata test-case ThirtyDays succeeded on Emilys-MBP

If the test fails it might look like this:

    Using local queues for Application Gilded Rose Refactoring Kata
    Q: Submitting Gilded Rose Refactoring Kata test-case ThirtyDays to default local queue
    S: Gilded Rose Refactoring Kata test-case ThirtyDays FAILED on Emilys-MBP : differences in stdout
    View details(v), Approve(a) or continue(any other key)?


If you press 'v' it will try to open the diff tool you specified in 'config.gr' to show you the difference in output. If you press 'a' it will update the approved file - you will not want to do this if you are refactoring. Any other key will return you to the terminal prompt.

## TextTest user interface

TextTest has a graphical user interface you can use to manage your test cases. With only one test case it may not be worth it, but if you want to add other tests and/or examine test failures more closely it can be helpful. Be sure to set TEXTTEST_HOME to the root folder of this repository before starting the GUI.

There are instructions for installing TextTest development tools on [texttest.org](https://texttest.org/)

## Running the test without TextTest

This should be perfectly possible, but is probably less convenient than using TextTest. 

Write a script that will execute the system under test (see "config.gr" for details of the executables), giving the commandline options listed in "options.gr". Collect the output from standard output in a file, and diff that against the golden copy "stdout.gr". Any diff is a test failure.

## Explaining TextTest test cases

Under the 'texttests' folder each test case has its own subdirectory. The name of the directory is the name of the test - in this case "ThirtyDays". The approved version of the output for that test case is kept in that directory. In this case we have three files:

- __stderr.gr__ - the expected output to Standard Error (stderr)
- __stdout.gr__ - the expected output to Standard Output (stdout)
- __options.gr__ - the options to give on the command line when you run the System Under Test (SUT)

In the directory above, there are configuration files for TextTest:

- __config.gr__ - this tells TextTest where to find the SUT executable, and sets up options for how it runs the SUT and interprets the output.
- __environment.gr__ - this file lists environment variables that will be set before TextTest runs the SUT. This is especially important for Java applications, that need to set the CLASSPATH environment variable in order to run properly.
- __testsuite.gr__ - lists the constituent test cases of this suite. Change the order of the entries here to change the order they appear in the TextTest GUI.

To run a test, click on it in the GUI and select "Run". TextTest will run it in a temporary (sandbox) directory and report the results. If the test fails, you can double click on a file to see the diff against the Golden Copy.

If you run into difficulties with TextTest, there is documentation available on [texttest.org](http://texttest.org), or you can ask a question on the [mailing list](https://lists.sourceforge.net/lists/listinfo/texttest-users).

## Introduction to Text-Based Approval Testing

This is a testing approach which is very useful when refactoring legacy code. Before you change the code, you run it, and gather the output of the code as a plain text file. You review the text, and if it correctly describes the behaviour as you understand it, you can "approve" it, and save it as a "Golden Master". Then after you change the code, you run it again, and compare the new output against the Golden Master. Any differences, and the test fails.

It's basically the same idea as "assertEquals(expected, actual)" in a unit test, except the text you are comparing is typically much longer, and the "expected" value is saved from actual output, rather than being defined in advance.

Typically a piece of legacy code may not produce suitable textual output from the start, so you may need to modify it before you can write your first text-based approval test. One way to do that is to write a "main" method that executes the code and prints out what the result is afterwards. Each language version has implemented a texttest 'fixture' that does this. It runs the GildedRose 'update_quality' method once each day for 30 days, printing the item state each day.

