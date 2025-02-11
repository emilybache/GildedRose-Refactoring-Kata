include(FetchContent)

FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG v1.16.0
  GIT_SHALLOW TRUE
)

FetchContent_Declare(
  catch2 
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG v3.8.0
  GIT_SHALLOW TRUE
)

FetchContent_MakeAvailable(googletest catch2)

# Force Google Test to link the C/C++ runtimes dynamically
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

# Disable building GMock
set(BUILD_GMOCK OFF CACHE BOOL "" FORCE)

# Do not install GTest 
set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)

add_subdirectory(cpp_catch2_approvaltest)
add_subdirectory(cpp_catch2_unittest)
add_subdirectory(cpp_googletest_approvaltest)
add_subdirectory(cpp_googletest_unittest)
add_subdirectory(cpp_texttest)
