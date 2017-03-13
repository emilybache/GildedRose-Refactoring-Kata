#include <check.h>
#include "GildedRose.h"



START_TEST(roseFoo)
{
  Item items[1];
  init_item(items, "foo", 0, 0);
  update_quality(items, 1);

  ck_assert_str_eq("fixme", items[0].name);
}
END_TEST

TCase *tcase_rose(void)
{
  TCase *tc;

  tc = tcase_create("gilded-rose");
  tcase_add_test(tc, roseFoo);

  return tc;
}

Suite *suite_rose(void)
{
  Suite *s;

  s = suite_create("characterization-tests");
  suite_add_tcase(s, tcase_rose());

  return s;
}
