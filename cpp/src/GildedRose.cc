#include "GildedRose.h"

GildedRose::GildedRose(vector<Item>& items) : items(items)
{

}

void GildedRose::updateQualityAgedBrie(int index)
{
    if (items[index].quality < 50)
    {
        items[index].quality += 1;
    }
    items[index].sellIn -= 1;
    if (items[index].sellIn < 0 && items[index].quality<50)
    {
        items[index].quality += 1;
    }
}
void GildedRose::updateQualityBackstagePasses(int index)
{
    if (items[index].quality < 50)
    {
        items[index].quality += 1;

        if (items[index].sellIn <= 10 && items[index].quality < 50)
        {
            items[index].quality += 1;
        }
        if (items[index].sellIn <= 5 && items[index].quality < 50)
        {
            items[index].quality += 1;
        }
    }
    items[index].sellIn -= 1;
    if (items[index].sellIn < 0)
    {
        items[index].quality = 0;
    }
    
}
void GildedRose::updateQualitySulfuras(int index)
{
}
void GildedRose::updateQualityConjured(int index)
{
    if (items[index].quality > 0)
    {
        items[index].quality -= 2;
    }
    items[index].sellIn -= 1;
    if (items[index].sellIn < 0 && items[index].quality>0)
    {
        items[index].quality -= 2;
    }
    if (items[index].quality < 0)
    {
        items[index].quality = 0;
    }
}
void GildedRose::updateQualityNormal(int index)
{
    if (items[index].quality > 0)
    {
        items[index].quality -= 1;
       
    }
    items[index].sellIn -= 1;
    if (items[index].sellIn < 0 && items[index].quality>0)
    {
        items[index].quality -= 1;
    }
}

void GildedRose::updateQuality()
{
    for (int itemIndex = 0; itemIndex < items.size(); itemIndex++)
    {

        if (items[itemIndex].name == "Aged Brie")
            updateQualityAgedBrie(itemIndex);
        else if (items[itemIndex].name == "Backstage passes to a TAFKAL80ETC concert")
            updateQualityBackstagePasses(itemIndex);
        else if (items[itemIndex].name == "Sulfuras, Hand of Ragnaros")
            updateQualitySulfuras(itemIndex);
        else if (items[itemIndex].name == "Conjured Mana Cake")
            updateQualityConjured(itemIndex);
        else
            updateQualityNormal(itemIndex);
        
    }
}
