#include <gtest/gtest.h>
#include "GildedRose.h"

TEST(GildedRoseTest, Foo) {
    vector<Item> items;
    items.push_back(Item("Foo", 0, 0));
    GildedRose app(items);
    app.updateQuality();
    EXPECT_EQ("fixme", app.items[0].name);
}
