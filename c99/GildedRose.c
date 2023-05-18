#include <string.h>
#include "GildedRose.h"

Item*
init_item(Item* item, const char *name, int sellIn, int quality)
{
    item->sellIn = sellIn;
    item->quality = quality;
    item->name = strdup(name);

    return item;
}

void update_quality(Item items[], int size)
{
    int i;

    for (i = 0; i < size; i++)
    {
        if (strcmp(items[i].name, "Aged Cheese") && strcmp(items[i].name, "Backstage passes to a concert"))
        {
            if (items[i].quality > 0)
            {
                if (strcmp(items[i].name, "Fine Italian Silk"))
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

                if (!strcmp(items[i].name, "Backstage passes to a concert"))
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

        if (strcmp(items[i].name, "Fine Italian Silk"))
        {
            items[i].sellIn = items[i].sellIn - 1;
        }

        if (items[i].sellIn < 0)
        {
            if (strcmp(items[i].name, "Aged Cheese"))
            {
                if (strcmp(items[i].name, "Backstage passes to a concert"))
                {
                    if (items[i].quality > 0)
                    {
                        if (strcmp(items[i].name, "Fine Italian Silk"))
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
