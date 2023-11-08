set(TEST_NAME test-cmocka)

include(FetchContent)

FetchContent_Declare(
        cmocka
        GIT_REPOSITORY https://git.cryptomilk.org/projects/cmocka.git
        GIT_TAG cmocka-1.1.5
        GIT_SHALLOW 1
)

set(WITH_STATIC_LIB ON CACHE BOOL "CMocka: Build with a static library" FORCE)
set(WITH_CMOCKERY_SUPPORT OFF CACHE BOOL "CMocka: Install a cmockery header" FORCE)
set(WITH_EXAMPLES OFF CACHE BOOL "CMocka: Build examples" FORCE)
set(UNIT_TESTING OFF CACHE BOOL "CMocka: Build with unit testing" FORCE)
set(PICKY_DEVELOPER OFF CACHE BOOL "CMocka: Build with picky developer flags" FORCE)

FetchContent_MakeAvailable(cmocka)

function(ADD_CMOCKA_TEST_ENVIRONMENT _TARGET_NAME)
    if (WIN32 OR CYGWIN OR MINGW OR MSVC)
        file(TO_NATIVE_PATH "${cmocka-library_BINARY_DIR}" CMOCKA_DLL_PATH)

        if (TARGET_SYSTEM_EMULATOR)
            set(DLL_PATH_ENV "WINEPATH=${CMOCKA_DLL_PATH};$ENV{WINEPATH}")
        else ()
            set(DLL_PATH_ENV "PATH=${CMOCKA_DLL_PATH}\\${CMAKE_BUILD_TYPE};$ENV{PATH}")
        endif ()
        #
        # IMPORTANT NOTE: The set_tests_properties(), below, internally
        # stores its name/value pairs with a semicolon delimiter.
        # because of this we must protect the semicolons in the path
        #
        string(REPLACE ";" "\\;" DLL_PATH_ENV "${DLL_PATH_ENV}")

        set_tests_properties(${_TARGET_NAME}
                PROPERTIES
                ENVIRONMENT
                "${DLL_PATH_ENV}")
    endif ()
endfunction()


add_subdirectory(sample_test)
