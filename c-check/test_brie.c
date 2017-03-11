#include <check.h>
#include "GildedRose.h"

#define CHEESE "Aged Brie"

START_TEST(agedBrie_whenSellInPositive_increasesQualityByOne)
{
  Item items[1];
  init_item(items, CHEESE, 10, 15);
  update_quality(items, 1);

  ck_assert_int_eq(16, items[0].quality);
}
END_TEST

START_TEST(agedBrie_cannotIncreaseQualityAboveFifty)
{
  Item items[1];
  init_item(items, CHEESE, 10, 50);
  update_quality(items, 1);

  ck_assert_int_eq(50, items[0].quality);
}
END_TEST

TCase *tcase_brie(void)
{
  TCase *tc;

  tc = tcase_create("aged-brie");
  tcase_add_test(tc, agedBrie_whenSellInPositive_increasesQualityByOne);
  tcase_add_test(tc, agedBrie_cannotIncreaseQualityAboveFifty);

  return tc;
}
