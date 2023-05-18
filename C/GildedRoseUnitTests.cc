#include <CppUTest/TestHarness.h>
#include <CppUTest/CommandLineTestRunner.h>
#include <CppUTestExt/MockSupport.h>

extern "C" {
#include "GildedRose.h"
}

TEST_GROUP(TestGildedRoseGroup)
{
  void setup() {
  }
  void teardown() {
  }
};

TEST(TestGildedRoseGroup, FirstTest)
{
    Item items[1];
    init_item(items, "Foo", 0, 0);
    update_quality(items, 1);
    STRCMP_EQUAL("fixme", items[0].name);
}

void example()
{
    Item items[6];
    int last = 0;

    init_item(items + last++, "Sports Memorabilia", 10, 20);
    init_item(items + last++, "Aged Cheese", 2, 0);
    init_item(items + last++, "Coffee Table Book", 5, 7);
    init_item(items + last++, "Fine Italian Silk", 0, 80);
    init_item(items + last++, "Backstage passes to a concert", 15, 20);
    init_item(items + last++, "Baked Chocolate Cake", 3, 6);
    update_quality(items, last);
}

int
main(int ac, char** av)
{
  return CommandLineTestRunner::RunAllTests(ac, av);
}
