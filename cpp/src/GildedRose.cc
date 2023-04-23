#include "GildedRose.h"

GildedRose::GildedRose(vector<Item> & items) : items(items)
{
}

void updateQuality(int q, int value)
{
    if(0 < q && q < 50)
    {
        q += value;
    }
}

void GildedRose::update()
{
    for (auto &item: this->items)
    {
        item.sellIn -= (item.name == "Sulfuras, Hand of Ragnaros") ? 0 : 1;

        if (item.name == "Aged Brie")
        {
            updateQuality(item.quality, 1);
            if (item.sellIn < 0)
            {
                updateQuality(item.quality, 1);
            }
        }
        else if (item.name == "Backstage passes to a TAFKAL80ETC concert")
        {
            if (item.sellIn < 0)
            {
                updateQuality(item.quality, -item.quality);
            }
            else if (item.sellIn < 5)
            {
                updateQuality(item.quality, 3);
            }
            else if (item.sellIn < 10)
            {
                updateQuality(item.quality, 2);
            }
            else
            {
                updateQuality(item.quality, 1);
            }
        }
        else if (item.name == "Sulfuras, Hand of Ragnaros")
        {
            updateQuality(item.quality, 0);
        }
        else if (item.sellIn < 0)
        {
            updateQuality(item.quality, -2);
        }
        else
        {
            updateQuality(item.quality, -1);
        }
    }
}
