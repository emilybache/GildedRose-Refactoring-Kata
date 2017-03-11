#include <check.h>
#include "GildedRose.h"

#define NORMAL_ITEM "Elixer of Mongoose"


START_TEST(normalitem_whenSellInPositive_decreasesQualityByOne)
{
  Item items[1];
  init_item(items, NORMAL_ITEM, 10, 15);
  update_quality(items, 1);

  ck_assert_int_eq(14, items[0].quality);
}
END_TEST

START_TEST(normalitem_whenSellIsZero_decreasesQualityByOne)
{
  Item items[1];
  init_item(items, NORMAL_ITEM, 0, 15);
  update_quality(items, 1);

  ck_assert_int_eq(70, items[0].quality);
}
END_TEST

TCase *tcase_rose(void)
{
  TCase *tc;

  tc = tcase_create("normal-items");
  tcase_add_test(tc, normalitem_whenSellInPositive_decreasesQualityByOne);

  return tc;
}

Suite *suite_rose(void)
{
  Suite *s;

  s = suite_create("characterization-tests");
  suite_add_tcase(s, tcase_rose());

  return s;
}
