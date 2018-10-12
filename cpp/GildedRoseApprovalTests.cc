#include "ApprovalTests.v.2.0.0.hpp"
#include <gtest/gtest.h>
#include "GildedRose.h"

std::ostream& operator<<(std::ostream& os, const Item& obj)
{
    return os
        << "name: " << obj.name
        << ", sellIn: " << obj.sellIn
        << ", quality: " << obj.quality;
}

TEST(GildedRoseApprovalTests, VerifyCombinations)
{
    std::vector<string> names { "Foo" };
    std::vector<int> sellIns { 1 };
    std::vector<int> qualities { 1 };

    CombinationApprovals::verifyAllCombinations<
        std::vector<string>, std::vector<int>, std::vector<int>, Item>(
                [](string name, int sellIn, int quality) {
                vector<Item> items = {Item(name, sellIn, quality)};
                GildedRose app(items);
                app.updateQuality();
                return items[0];
                },
                names, sellIns, qualities);
}
