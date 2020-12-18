# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.18.20081302-MSVC_2)

# We name the project and the target for the ExternalProject_Add() call
# to something that will highlight to the user what we are working on if
# something goes wrong and an error message is produced.

project(catch2-populate NONE)

include(ExternalProject)
ExternalProject_Add(catch2-populate
                     "UPDATE_DISCONNECTED" "False" "GIT_REPOSITORY" "https://github.com/catchorg/Catch2.git" "GIT_TAG" "v2.12.2"
                    SOURCE_DIR          "C:/Users/np702/Documents/Harmi/CommitProject/PatelHarmi/GildedRose-Refactoring-Kata/cpp-catch2/out/build/x64-Debug (default)/_deps/catch2-src"
                    BINARY_DIR          "C:/Users/np702/Documents/Harmi/CommitProject/PatelHarmi/GildedRose-Refactoring-Kata/cpp-catch2/out/build/x64-Debug (default)/_deps/catch2-build"
                    CONFIGURE_COMMAND   ""
                    BUILD_COMMAND       ""
                    INSTALL_COMMAND     ""
                    TEST_COMMAND        ""
                    USES_TERMINAL_DOWNLOAD  YES
                    USES_TERMINAL_UPDATE    YES
)


