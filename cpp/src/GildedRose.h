#include <string>
#include <vector>

using namespace std;

class Item
{
public:
    string name;
    int sellIn;
    int quality;
    Item(string name, int sellIn, int quality) : name(name), sellIn(sellIn), quality(quality)
    {}
};

class GildedRose
{
public:
    vector<Item>& items;
    GildedRose(vector<Item>& items);

    void updateQuality();
    void updateQualityAgedBrie(int index);
    void updateQualityBackstagePasses(int index);
    void updateQualityConjured(int index);
    void updateQualitySulfuras(int index);
    void updateQualityNormal(int index);
};
