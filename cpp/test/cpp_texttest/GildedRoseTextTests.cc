#include <iostream>
#include "GildedRose.h"

void print_item(Item& item)
{
    std::cout << item.name << ", " <<  item.sellIn << ", " << item.quality << std::endl;
}

int main()
{
    vector<Item> items;

    items.push_back({"Sports Memorabilia", 10, 20});
    items.push_back({"Aged Cheese", 2, 0});
    items.push_back({"Coffee Table Book", 5, 7});
    items.push_back({"Fine Italian Silk", 0, 80});
    items.push_back({"Fine Italian Silk", -1, 80});
    items.push_back({"Backstage passes to a concert", 15, 20});
    items.push_back({"Backstage passes to a concert", 10, 49});
    items.push_back({"Backstage passes to a concert", 5, 49});

    // this Baked item doesn't yet work properly
    items.push_back({"Baked Chocolate Cake", 3, 6});

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


