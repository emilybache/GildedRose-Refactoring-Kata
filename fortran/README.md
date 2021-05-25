# Fortran version of Gilded Rose refactoring kata

## Introduction
The Fortran90 version of the Gilded Rose refactoring kata.

## Prerequisites

* CMake version >= 3.13
* Fortran compiler
    * Tested with:
        * gfortran
        * Intel Fortran

## How to build and run tests in a terminal

### Build tests

    $ cd ${GIT_FOLDER}/GildedRose-Refactoring-Kata/fortran
    $ mkdir build
    $ cd build
    $ cmake ..
    $ cmake --build .

### Show available tests

    $ cd ${GIT_FOLDER}/GildedRose-Refactoring-Kata/fortran/build
    $ ctest -N
    Test project ${GIT_FOLDER}/GildedRose-Refactoring-Kata/fortran/build
      Test #1: GildedRose_text_test
      Test #2: GildedRose_unity_test

    Total Tests: 2

### Run all tests

    $ ctest

### Run all tests with verbose output

    $ ctest -VV

## How to build and run tests using the [CLion IDE](https://www.jetbrains.com/clion/)

1. Start CLion
2. Select menu `File - Open...`
3. Select folder `${GIT_FOLDER}/GildedRose-Refactoring-Kata/fortran`
4. Select menu `Build - Build Project`
4. Select menu `Run - Run...`

## How to build and run tests using Visual Studio 2019 

1. Start Visual Studio 2019
2. Select `Open a local folder`
3. Select folder `${GIT_FOLDER}/GildedRose-Refactoring-Kata/fortran`
4. Wait for message `CMake generation finished.` in the CMake output window at the bottom
5. Select ALL_BUILD and build to build the source code
6. Select RUN_TEST and build which will execute the tests
