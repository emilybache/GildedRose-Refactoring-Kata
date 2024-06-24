#include <iostream>
#include "GildedRose.h"

void print_item(Item& item)
{
    std::cout << item.name << ", " <<  item.sellIn << ", " << item.quality << std::endl;
}

int main()
{
    vector<Item> items;

    items.push_back({"+5 Dexterity Vest", 10, 20});
    items.push_back({"Aged Brie", 2, 0});
    items.push_back({"Elixir of the Mongoose", 5, 7});
    items.push_back({"Sulfuras, Hand of Ragnaros", 0, 80});
    items.push_back({"Sulfuras, Hand of Ragnaros", -1, 80});
    items.push_back({"Backstage passes to a TAFKAL80ETC concert", 15, 20});
    items.push_back({"Backstage passes to a TAFKAL80ETC concert", 10, 49});
    items.push_back({"Backstage passes to a TAFKAL80ETC concert", 5, 49});

    // this Conjured item doesn't yet work properly
    items.push_back({"Conjured Mana Cake", 3, 6});

	std::cout << "OMGHAI!" << std::endl;

    GildedRose app(items);

    for (int day = 0; day <= 30; day++)
    {
		std::cout << "-------- day " << day << " --------" << std::endl;
		std::cout << "name, sellIn, quality" << std::endl;
        for (auto& item : items)
        {
            print_item(item);
        }
		std::cout << std::endl;
        app.updateQuality();
    }
    return 0;
}


