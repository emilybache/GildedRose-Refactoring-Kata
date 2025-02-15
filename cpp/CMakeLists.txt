cmake_minimum_required(VERSION 3.13)
project(gilded-rose-refactoring-kata-cpp)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# uncomment this line to enable coverage measurements using gcov
# set(CMAKE_CXX_FLAGS "--coverage")

option(BUILD_APPROVAL_TESTS_WITH_GTEST "Use GoogleTest for approval testing" ON)

option(BUILD_UNIT_TESTS_WITH_GTEST "Use GoogleTest for unit testing" ON)

option(BUILD_APPROVAL_TESTS_WITH_CATCH2 "Use Catch2 for approval testing" ON)

option(BUILD_UNIT_TESTS_WITH_CATCH2 "Use Catch2 for unit testing" ON)

enable_testing()
add_subdirectory(src)
add_subdirectory(test)
