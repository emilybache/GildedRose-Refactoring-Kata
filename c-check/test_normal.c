#include <check.h>
#include "GildedRose.h"

#define NORMAL_ITEM "Elixer of Mongoose"

TCase *tcase_brie(void);
TCase *tcase_backstage(void);

START_TEST(normalitem_whenSellInPositive_decreasesQualityByOne)
{
  Item items[1];
  init_item(items, NORMAL_ITEM, 10, 15);
  update_quality(items, 1);

  ck_assert_int_eq(14, items[0].quality);
}
END_TEST

START_TEST(normalitem_whenSellIsZero_decreasesQualityByTwo)
{
  Item items[1];
  init_item(items, NORMAL_ITEM, 0, 15);
  update_quality(items, 1);

  ck_assert_int_eq(13, items[0].quality);
}
END_TEST

START_TEST(normalitem_whenQualityZero_doesNotDecrease)
{
  Item items[1];
  init_item(items, NORMAL_ITEM, 10, 0);
  update_quality(items, 1);

  ck_assert_int_eq(0, items[0].quality);
}
END_TEST


START_TEST(normalitem_whenSellInNegative_decreasesByTwo)
{
  Item items[1];
  init_item(items, NORMAL_ITEM, -1, 20);
  update_quality(items, 1);

  ck_assert_int_eq(18, items[0].quality);
}
END_TEST

START_TEST(normalitem_whenSellInZero_decreasesByTwo)
{
  Item items[1];
  init_item(items, NORMAL_ITEM, -1, 15);
  update_quality(items, 1);

  ck_assert_int_eq(13, items[0].quality);
}
END_TEST

TCase *tcase_normal(void)
{
  TCase *tc;

  tc = tcase_create("normal-items");
  tcase_add_test(tc, normalitem_whenSellInPositive_decreasesQualityByOne);
  tcase_add_test(tc, normalitem_whenSellIsZero_decreasesQualityByTwo);
  tcase_add_test(tc, normalitem_whenQualityZero_doesNotDecrease);
  tcase_add_test(tc, normalitem_whenSellInNegative_decreasesByTwo);
  tcase_add_test(tc, normalitem_whenSellInZero_decreasesByTwo);

  return tc;
}

Suite *suite_normal(void)
{
  Suite *s;

  s = suite_create("characterization-tests");
  suite_add_tcase(s, tcase_normal());
  suite_add_tcase(s, tcase_brie());
  suite_add_tcase(s, tcase_backstage());

  return s;
}
