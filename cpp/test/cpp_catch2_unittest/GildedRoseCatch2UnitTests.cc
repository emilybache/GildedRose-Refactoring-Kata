#include <catch2/catch_all.hpp>
#include "GildedRose.h"

TEST_CASE("GildedRoseUnitTest", "Foo")
{
    std::vector<Item> items;
    items.push_back(Item("Foo", 0, 0));
    GildedRose app(items);
    app.updateQuality();
    REQUIRE("fixme" == app.items[0].name);
}
