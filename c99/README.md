# Gilded Rose, C99 Edition

The command "make" will build and run your tests, as well as build the program
golden_rose, which can serve as the basis for a golden-rule test.


## Assumptions
  - gnu make and a C compiler (like gcc) is installed on your system and is in the PATH
  - The check unit testing library is installed on your system (https://libcheck.github.io/check/)
  - pkg-config is installed on your system

## Usage
  - Run `make` to build the program and run all tests
  - Files which contain tests should be named `test_*.c`  They will automatically
    be included in your test suite.
  - `GildedRose.h` should not be modified.  The Goblin threat is real.
  - New program logic may be included in files named `gilded_*.c` which will
    automatically be included in both your tests and the final program.

## Golden Rule tests
  - The program `golden_rose` will generate text output.  If you capture this
    output after your first `make` you can use this as a reference for a golden
    rule test.
  - You can test your work against this reference by directing the output of your
    current golden_rose to a file and using the `diff` utility to compare that
    to the reference file you created above.
  - To avoid the Goblin threat you can use `git diff GildedRose.h`, which should
    have no output if you have left the precious Item structure unchanged.

## Notes
  - This project is tweaked to run on Linux systems, and will mostly work on Macs.
    With some changes to the Makefile it can be made to run on BSD systems with
    BSD make.  An adventurous person could also get it to run on Windows.
  - If you are working on a Macintosh computer you cannot run the memtest target,
    because valgrind and OS X don't play nice any more.  If you want to use the
    memory checker OS X does run docker as a first class citizen.
  - If you don't have pkg-config on your system, the only changes you'll need to
    make are for the requirements of the check library.  Mostly you need to
    set the appropriate flags for threaded binaries, which may include some
    special linker flags.  The libcheck documentation will cover what you need
    if you want to undertake this change.
