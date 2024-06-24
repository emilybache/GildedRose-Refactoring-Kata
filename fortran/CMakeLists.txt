cmake_minimum_required(VERSION 3.0.0)
project(GildedRose Fortran)
include(CTest)

set(EXERCISM_RUN_ALL_TESTS 1)

# Activate Fortran compiler warnings
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel") # Intel fortran
  if(WIN32)
    set (CMAKE_Fortran_FLAGS "/warn:all")
  else()
    set (CMAKE_Fortran_FLAGS "-warn all")
  endif()
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU" ) # GFortran
#  set (CMAKE_Fortran_FLAGS "-std=f2008 -W -Wall -Wextra -pedantic -fbacktrace -Wdo-subscript")
  set (CMAKE_Fortran_FLAGS "-std=f2008 -W -Wall -Wextra -pedantic -fbacktrace ")
  set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS} -o0 -ffpe-trap=zero,invalid,overflow,underflow") 
endif()

add_executable(GildedRose_text_test
        src/GildedRose.f90
        test/GildedRose_text_test.f90
        )
add_executable(GildedRose_unity_test
        src/GildedRose.f90
        test/GildedRose_unity_test.f90
        )

  add_test(GildedRose_text_test GildedRose_text_test)
  add_test(GildedRose_unity_test GildedRose_unity_test)


