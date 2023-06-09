cmake_minimum_required(VERSION 3.13)
project(gilded-rose-refactoring-kata-cpp)

# Load FetchContent module.
include(FetchContent)

# Declare GoogleTest as the content to fetch
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG release-1.12.1
)

FetchContent_Declare(
  catch2 
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG v3.3.2
)

FetchContent_MakeAvailable(googletest catch2)

#set(gtest_force_shared_crt OFF CACHE BOOL "" FORCE)
# Force Google Test to link the C/C++ runtimes dynamically when
# building on Visual Studio
# Link:
# * https://github.com/google/googletest/tree/release-1.8.1/googletest#visual-studio-dynamic-vs-static-runtimes
if (MSVC)
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
endif()

# uncomment this line to enable coverage measurements using gcov
# set(CMAKE_CXX_FLAGS "--coverage")

enable_testing()
add_subdirectory(src)
add_subdirectory(lib)
add_subdirectory(test)
