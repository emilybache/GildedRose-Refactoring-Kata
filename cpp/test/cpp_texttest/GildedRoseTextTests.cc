#include <cstdio>
#include "GildedRose.h"

int
print_item(Item *item)
{
    return printf("%s, %d, %d\n", item->name.c_str(), item->sellIn, item->quality);
}

int main()
{
    vector<Item> items;

    items.emplace_back("+5 Dexterity Vest", 10, 20);
    items.emplace_back("Aged Brie", 2, 0);
    items.emplace_back("Elixir of the Mongoose", 5, 7);
    items.emplace_back("Sulfuras, Hand of Ragnaros", 0, 80);
    items.emplace_back("Sulfuras, Hand of Ragnaros", -1, 80);
    items.emplace_back("Backstage passes to a TAFKAL80ETC concert", 15, 20);
    items.emplace_back("Backstage passes to a TAFKAL80ETC concert", 10, 49);
    items.emplace_back("Backstage passes to a TAFKAL80ETC concert", 5, 49);

    // this Conjured item doesn't yet work properly
    items.emplace_back("Conjured Mana Cake", 3, 6);

    puts("OMGHAI!");

    GildedRose app(items);

    for (int day = 0; day <= 30; day++)
    {
        printf("-------- day %d --------\n", day);
        printf("name, sellIn, quality\n");
        for (auto & item : items)
        {
            print_item(&item);
        }
        printf("\n");
        app.updateQuality();
    }
    return 0;
}


