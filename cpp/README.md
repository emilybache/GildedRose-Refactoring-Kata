TL;DR;
-------
run-once.sh runs your tests once

Before this will work you will need:
  - make and a C++ compiler (like gcc) is installed on your system and is in the PATH
  - The GTest framework in the directory gtest.
  - If your IDE does the compilation and linking, you should remove the first 3 lines
    in the run-once.sh file.

More Verbose Instructions
-------------------------

Create a clone of both GildedRose-Refactoring-Kata and googletest in a directory we'll call ${ROOT_INSTALL_DIR}:

    cd ${ROOT_INSTALL_DIR}
    git clone https://github.com/emilybache/GildedRose-Refactoring-Kata
    git clone https://github.com/google/googletest

Make googletest by running make in subfolder googletest/googletest/make:

    cd googletest/googletest/make
    make

Create a softlink in the GildedRose-Refactoring-Kata clone pointing at the googletest code:

    cd ${ROOT_INSTALL_DIR}/GildedRose-Refactoring-Kata/cpp
    ln -s ${ROOT_INSTALL_DIR}/googletest/googletest gtest

Make the GildedRose-Refactoring-Kata:

    make

Then you should be able to run the tests:

    ./run_once.sh

If you have been successful, then you should see a failing test, "GildedRoseTest.Foo".
