# Gilded Rose starting position in C with CMocka

Use CMake to build it. Run the target "sample_test" to run the sample unit test using the cmocka framework.

## Run the TextTest fixture on the command line

When you build this project this executable should be created:

    c_cmocka/cmake-build-debug/main

Execute it on the command line with an argument for the number of days:

    c_cmocka/cmake-build-debug/main 10

## Run the TextTest approval test that comes with this project

There are instructions in the [TextTest Readme](../texttests/README.md) for setting up TextTest. You will need to specify the C executable in [config.gr](../texttests/config.gr). Uncomment this line to use it:

    executable:${TEXTTEST_HOME}/c_cmocka/cmake-build-debug/main

