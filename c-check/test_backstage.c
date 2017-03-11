#include <check.h>
#include "GildedRose.h"

#define GOOD_BACKSTAGE "Backstage passes to a TAFKAL80ETC concert"
#define BAD_BACKSTAGE "Backstage passes to a Jeff Beck concert"

START_TEST(backstage_whenMoreThan10Days_increasesByOne)
{
  Item items[1];
  init_item(items, GOOD_BACKSTAGE, 12, 15);
  update_quality(items, 1);

  ck_assert_int_eq(16, items[0].quality);
}
END_TEST

START_TEST(backstage_when10Days_increasesByTwo)
{
  Item items[1];
  init_item(items, GOOD_BACKSTAGE, 10, 15);
  update_quality(items, 1);

  ck_assert_int_eq(17, items[0].quality);
}
END_TEST

START_TEST(backstage_when5Days_increasesByThree)
{
  Item items[1];
  init_item(items, GOOD_BACKSTAGE, 5, 15);
  update_quality(items, 1);

  ck_assert_int_eq(18, items[0].quality);
}
END_TEST

START_TEST(backstage_when0Days_hasQualityZero)
{
  Item items[1];
  init_item(items, GOOD_BACKSTAGE, 0, 15);
  update_quality(items, 1);

  ck_assert_int_eq(0, items[0].quality);
}
END_TEST

START_TEST(otherBackstage_when12Days_decreasesByOne)
{
  Item items[1];
  init_item(items, BAD_BACKSTAGE, 12, 15);
  update_quality(items, 1);

  ck_assert_int_eq(14, items[0].quality);
}
END_TEST

TCase *tcase_backstage(void)
{
  TCase *tc;

  tc = tcase_create("backstage-pass");
  tcase_add_test(tc, backstage_whenMoreThan10Days_increasesByOne);
  tcase_add_test(tc, backstage_when10Days_increasesByTwo);
  tcase_add_test(tc, backstage_when5Days_increasesByThree);
  tcase_add_test(tc, backstage_when0Days_hasQualityZero);
  tcase_add_test(tc, otherBackstage_when12Days_decreasesByOne);

  return tc;
}
