# C++ version of Gilded Rose refactoring kata

## Introduction
The C++ version of the Gilded Rose refactoring kata is available in four variants using different test frameworks:

* Catch2 test framework
  1. Traditional unit test with the [Catch2](https://github.com/catchorg/Catch2) test framework in the `test/cpp_catch2_unittest` folder.
  2. [Approval tests](https://github.com/approvals/ApprovalTests.cpp) with the [Catch2](https://github.com/catchorg/Catch2) test framework in the `test/cpp_catch2_approvaltest` folder.
* Google Test framework
  1. Traditional unit test with the [Googletest](https://github.com/google/googletest) test framework in the `test/cpp_googletest_unittest` folder.
  2. [Approval tests](https://github.com/approvals/ApprovalTests.cpp) with the [Googletest](https://github.com/google/googletest) test framework in the `test/cpp_googletest_approvaltest` folder.

The `GildedRose.cc` file, i.e. the code under test, is identical in all four variants.

## Prerequisites

* CMake version >= 3.13
* C++ compiler that support C++11

## How to build and run tests in a terminal

### Build tests

    $ cd ${GIT_FOLDER}/GildedRose-Refactoring-Kata/cpp
    $ mkdir build
    $ cd build
    $ cmake ..
    $ cmake --build .

### Show available tests

    $ cd ${GIT_FOLDER}/GildedRose-Refactoring-Kata/cpp/build
    $ ctest -N
    Test project ${GIT_FOLDER}/GildedRose-Refactoring-Kata/cpp/build
      Test #1: GildedRoseCatch2ApprovalTests
      Test #2: GildedRoseCatch2UnitTests
      Test #3: GildedRoseGoogletestApprovalTests
      Test #4: GildedRoseGoogletestUnitTests

### Run all tests

    $ ctest

### Run all tests with verbose output

    $ ctest -VV

### Run a specific test with verbose output

    $ ctest -VV --tests-regex Catch2Approval

## How to build and run tests using the [CLion IDE](https://www.jetbrains.com/clion/)

1. Start CLion
2. Select menu `File - Open...`
3. Select folder `${GIT_FOLDER}/GildedRose-Refactoring-Kata/cpp`
4. Select menu `Build - Build Project`
4. Select menu `Run - Run...`
4. Select what test variant to run, e.g. `GildedRoseCatch2ApprovalTests`.

## How to build and run tests using Visual Studio 2019 

1. Start Visual Studio 2019
2. Select `Open a local folder`
3. Select folder `${GIT_FOLDER}/GildedRose-Refactoring-Kata/cpp`
4. Wait for message `CMake generation finished.` in the CMake output window at the bottom
5. Select what test variant to run in the drop down menu for Startup Items, e.g. `GildedRoseCatch2ApprovalTests.exe`.
6. Select menu `Debug - Start`
