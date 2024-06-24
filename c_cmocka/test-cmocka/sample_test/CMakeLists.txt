project(sample-cmocka C)

add_cmocka_test(sample_test
        SOURCES sample_test.c
        LINK_LIBRARIES src cmocka-static)
add_cmocka_test_environment(add_test)
