#include "unity.h"
#include "GildedRose.h"

void setUp (void) {} /* Is run before every test, put unit init calls here. */
void tearDown (void) {} /* Is run after every test, put unit clean-up calls here. */

void test_NameOfItem(void)
{
    Item items[1];
    init_item(items, "Foo", 0, 0);
    update_quality(items, 1);
    TEST_ASSERT_EQUAL_STRING( "fixme", items[0].name );
}

int main(void)
{
    UNITY_BEGIN();
    RUN_TEST(test_NameOfItem);
    return UNITY_END();
}
