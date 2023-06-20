cmake_minimum_required(VERSION 3.14..3.16)
set(CMAKE_VERBOSE_MAKEFILE ON)
project(GildedRoseKata VERSION 1.0
        DESCRIPTION "The GildedRose Refactoring kata for an approval testing approach"
        LANGUAGES CXX)
include(FetchContent)

FetchContent_Declare(
        catch2
        GIT_REPOSITORY https://github.com/catchorg/Catch2.git
        GIT_TAG v2.13.9
)
FetchContent_MakeAvailable(catch2)
LIST(APPEND CMAKE_MODULE_PATH
        ${catch2_SOURCE_DIR}/contrib
        )

FetchContent_Declare(
        approvaltests_ho
        URL https://github.com/approvals/ApprovalTests.cpp/releases/download/v.10.0.0/ApprovalTests.v.10.0.0.hpp
        DOWNLOAD_NO_EXTRACT TRUE
        DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}/approvaltests
        DOWNLOAD_NAME ApprovalTests.v.10.0.0.hpp
)
FetchContent_GetProperties(approvaltests_ho)
if (NOT approvaltests_ho_POPULATED)
    FetchContent_Populate(approvaltests_ho)
endif ()
add_library(approvaltests INTERFACE)
target_include_directories(approvaltests
        INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/approvaltests
        INTERFACE ${catch2_SOURCE_DIR}/single_include/catch2
        )

if( MINGW ) 
  # https://stackoverflow.com/questions/31890021/mingw-too-many-sections-bug-while-compiling-huge-header-file-in-qt
  set (CMAKE_CXX_FLAGS "-Wa,-mbig-obj ")
endif()

add_executable(gildedrose_catch2
        src/GildedRose.h
        src/GildedRose.cc
        test/gildedrose_catch.cpp
        test/main.cpp)
set_target_properties(gildedrose_catch2 PROPERTIES CXX_STANDARD 11)
target_include_directories(gildedrose_catch2
        PUBLIC src)
target_link_libraries(gildedrose_catch2 Catch2::Catch2 approvaltests)



include(CTest)
include(ParseAndAddCatchTests)
ParseAndAddCatchTests(gildedrose_catch2)
