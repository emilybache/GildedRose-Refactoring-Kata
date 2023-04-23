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
    for (int i = 0; i < items.size(); i++)
    {
        items[i].sellIn -= (items[i].name == "Sulfuras, Hand of Ragnaros") ? 0 : 1;
        
        if (items[i].name == "Aged Brie")
        {
            updateQuality(items[i].quality, 1);
        }
        else if (items[i].name == "Backstage passes to a TAFKAL80ETC concert")
        {
            if (items[i].sellIn < 5)
            {
                updateQuality(items[i].quality, 3);
            }  
            else if (items[i].sellIn < 10)
            {
                updateQuality(items[i].quality, 2);
            }
            else
            {
                updateQuality(items[i].quality, 1);
            }
        }
        else if (items[i].name == "Sulfuras, Hand of Ragnaros")
        {
            updateQuality(items[i].quality, 0);
        }
        else
        {
            updateQuality(items[i].quality, -1);
        }

        if (items[i].sellIn < 0)
        {
            if (items[i].name == "Aged Brie")
            {
                updateQuality(items[i].quality, 1);
            }
            else if (items[i].name == "Backstage passes to a TAFKAL80ETC concert")
            {
                updateQuality(items[i].quality, -items[i].quality);
            }
            else if (items[i].name == "Sulfuras, Hand of Ragnaros")
            {
                updateQuality(items[i].quality, 0);
            }
            else
            {
                updateQuality(items[i].quality, -1);
            }
        }
    }
}
