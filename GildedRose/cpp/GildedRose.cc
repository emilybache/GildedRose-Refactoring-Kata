#include <gtest/gtest.h>

void example()
{
    std::list<Item> items = new std::list<Item>();
    items.push_back(new Item("+5 Dexterity Vest", 10, 20));
    items.push_back(new Item("Aged Brie", 2, 0));
    items.push_back(new Item("Elixir of the Mongoose", 5, 7));
    items.push_back(new Item("Sulfuras, Hand of Ragnaros", 0, 80));
    items.push_back(new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20));
    items.push_back(new Item("Conjured Mana Cake", 3, 6));
    GildedRose app = new GildedRose(items);
    app.updateQuality();
}

class GildedRose
{
public:
    std::list<Item> items;
    GildedRose(std::list items) 
    {
        this.items = items;
    }
    
    void updateQuality() 
    {
        for (int i = 0; i < items.length; i++)
        {
            if (items[i].name != "Aged Brie" && items[i].name != "Backstage passes to a TAFKAL80ETC concert")
            {
                if (items[i].quality > 0)
                {
                    if (items[i].name != "Sulfuras, Hand of Ragnaros")
                    {
                        items[i].quality = items[i].quality - 1;
                    }
                }
            }
            else
            {
                if (items[i].quality < 50)
                {
                    items[i].quality = items[i].quality + 1;

                    if (items[i].name == "Backstage passes to a TAFKAL80ETC concert")
                    {
                        if (items[i].sellIn < 11)
                        {
                            if (items[i].quality < 50)
                            {
                                items[i].quality = items[i].quality + 1;
                            }
                        }

                        if (items[i].sellIn < 6)
                        {
                            if (items[i].quality < 50)
                            {
                                items[i].quality = items[i].quality + 1;
                            }
                        }
                    }
                }
            }

            if (items[i].name != "Sulfuras, Hand of Ragnaros")
            {
                items[i].sellIn = items[i].sellIn - 1;
            }

            if (items[i].sellIn < 0)
            {
                if (items[i].name != "Aged Brie")
                {
                    if (items[i].name != "Backstage passes to a TAFKAL80ETC concert")
                    {
                        if (items[i].quality > 0)
                        {
                            if (items[i].name != "Sulfuras, Hand of Ragnaros")
                            {
                                items[i].quality = items[i].quality - 1;
                            }
                        }
                    }
                    else
                    {
                        items[i].quality = items[i].quality - items[i].quality;
                    }
                }
                else
                {
                    if (items[i].quality < 50)
                    {
                        items[i].quality = items[i].quality + 1;
                    }
                }
            }
        }
    }
};

class Item
{
public:
    std::string name;
    int sellIn;
    int quality;
};

TEST(GildedRoseTest, Foo) {
    std::list<Item> items = new std::list<Item>();
    items.push_back(new Item("Foo", 0, 0));
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    EXPECT_EQ("fixme", app.items[0].name);
}

