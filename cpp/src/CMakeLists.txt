set(SRC_LIB_NAME GildedRoseLib)
add_library(${SRC_LIB_NAME})
target_sources(${SRC_LIB_NAME} PRIVATE GildedRose.cc GildedRose.h)
target_include_directories(${SRC_LIB_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
