#include <stdarg.h>
#include <setjmp.h>
#include <stddef.h>
#include "cmocka.h"

#include "GildedRose.h"

static void test_rose(void **state)
{
    Item items[1];
    init_item(items, "foo", 0, 0);
    update_quality(items, 1);

    assert_string_equal("fixme", items[0].name);
}

int main(void) {
    const struct CMUnitTest tests[] =
            {
                    cmocka_unit_test(test_rose),
            };

    return cmocka_run_group_tests(tests, NULL, NULL);
}
