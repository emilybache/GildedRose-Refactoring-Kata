cmake_minimum_required(VERSION 2.8.4)
project(cpp)

# CMake FindThreads is broken until 3.1
#find_package(Threads REQUIRED)
set(CMAKE_THREAD_LIBS_INIT pthread)

enable_testing()
find_package(GTest)

include(ExternalProject)
if(NOT ${GTEST_FOUND}) # Download gtest if not installed
    message("No system gtest use external project")
    ExternalProject_Add(googletest
      GIT_REPOSITORY    https://github.com/google/googletest.git
      GIT_TAG           master
      SOURCE_DIR        "${CMAKE_BINARY_DIR}/googletest-src"
      BINARY_DIR        "${CMAKE_BINARY_DIR}/googletest-build"
      CONFIGURE_COMMAND ""
      BUILD_COMMAND     ""
      INSTALL_COMMAND   ""
      TEST_COMMAND      ""
    )
    add_subdirectory(${CMAKE_BINARY_DIR}/googletest-src
                 ${CMAKE_BINARY_DIR}/googletest-build
                 EXCLUDE_FROM_ALL)
    set(GTEST_BOTH_LIBRARIES gtest gtest_main)
endif()

ExternalProject_Add(aprovaltest
    PREFIX ${CMAKE_BINARY_DIR}/aprovaltest
    URL https://github.com/approvals/ApprovalTests.cpp/releases/download/v.2.0.0/ApprovalTests.v.2.0.0.hpp
    DOWNLOAD_NO_EXTRACT 1
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
)

include_directories(${GTEST_INCLUDE_DIRS})
include_directories(${CMAKE_BINARY_DIR}/aprovaltest/src)

add_executable(GildedRose
    GildedRose.cc
    GildedRose.h
    GildedRoseUnitTests.cc
    )
target_link_libraries(GildedRose ${GTEST_BOTH_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
add_test(NAME GildedRose COMMAND GildedRose)

add_executable(GildedRoseTextTests
    GildedRose.cc
    GildedRose.h
    GildedRoseTextTests.cc
    )
target_link_libraries(GildedRoseTextTests ${GTEST_BOTH_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
add_test(NAME GildedRoseTextTests COMMAND GildedRoseTextTests)

add_executable(GildedRoseApprovalTests
    GildedRose.cc
    GildedRose.h
    GildedRoseApprovalTests.cc
    GildedRoseApprovalMain.cc
    )
target_link_libraries(GildedRoseApprovalTests ${GTEST_BOTH_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
set_property(TARGET GildedRoseApprovalTests PROPERTY CXX_STANDARD 14)
add_dependencies(GildedRoseApprovalTests aprovaltest)
add_test(NAME GildedRoseApprovalTests COMMAND GildedRoseApprovalTests)
