#include "catch2/catch.hpp"
#include "ApprovalTests.hpp"

#include "GildedRose.h"

TEST_CASE("Foo") {

    vector<Item> items;
    items.push_back(Item("foo", 0, 0));
    GildedRose app(items);
    app.updateQuality();
    REQUIRE("fixme" == app.items[0].name);

}
