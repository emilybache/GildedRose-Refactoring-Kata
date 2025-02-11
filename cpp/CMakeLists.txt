cmake_minimum_required(VERSION 3.13)
project(gilded-rose-refactoring-kata-cpp)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# uncomment this line to enable coverage measurements using gcov
# set(CMAKE_CXX_FLAGS "--coverage")

enable_testing()
add_subdirectory(src)
add_subdirectory(test)
