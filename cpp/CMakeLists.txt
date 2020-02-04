cmake_minimum_required(VERSION 3.13)
project(gilded-rose-refactoring-kata-cpp)

# Load FetchContent module.
include(FetchContent)

# Declare GoogleTest as the content to fetch
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG release-1.8.1
)

# Fetch GoogleTest amd make build scripts available
if (NOT googletest_POPULATED)
  FetchContent_Populate(googletest)
endif()

#set(gtest_force_shared_crt OFF CACHE BOOL "" FORCE)
# Force Google Test to link the C/C++ runtimes dynamically when
# building on Visual Studio
# Link:
# * https://github.com/google/googletest/tree/release-1.8.1/googletest#visual-studio-dynamic-vs-static-runtimes
if (MSVC)
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
endif()

# Bring the populated content into the build
add_subdirectory(${googletest_SOURCE_DIR} ${googletest_BINARY_DIR})

# uncomment this line to enable coverage measurements using gcov
# set(CMAKE_CXX_FLAGS "--coverage")

enable_testing()
add_subdirectory(src)
add_subdirectory(lib)
add_subdirectory(test)
