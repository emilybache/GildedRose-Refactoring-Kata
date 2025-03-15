cmake_minimum_required(VERSION 3.14 FATAL_ERROR)

project(GildedRose VERSION 1.0)

set(CMAKE_C_STANDARD 11)

add_subdirectory(src)
add_subdirectory(test-cmocka)


add_executable(main GildedRoseTextTests.c)
target_link_libraries(main src)

