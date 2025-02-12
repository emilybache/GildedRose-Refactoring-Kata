#pragma once

#include <string>
#include <vector>

class Item
{
public:
    std::string name;
    int sellIn;
    int quality;
    Item(std::string name, int sellIn, int quality) : name(name), sellIn(sellIn), quality(quality) 
    {}
};

class GildedRose
{
public:
    std::vector<Item> & items;
    GildedRose(std::vector<Item> & items);
    
    void updateQuality();
};
