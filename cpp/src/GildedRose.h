#include <string>
#include <vector>

using namespace std;

class Item
{
    private:
        string name;
        int sellIn;
        int quality;
    
    public:
        Item(string name, int sellIn, int quality) : name(name), sellIn(sellIn), quality(quality) 
        {
        }
};

class GildedRose
{
    private:
        vector<Item> & items;
    
    public:
        GildedRose(vector<Item> & items);
        void updateQuality();
};

