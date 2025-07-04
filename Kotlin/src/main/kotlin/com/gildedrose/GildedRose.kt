package com.gildedrose

class GildedRose(val items: List<Item>) {

    fun updateQuality() {
        for (i in items.indices) {
            if (items[i].name == "Sulfuras, Hand of Ragnaros") items[i].quality = 80;
            else {
                    if (items[i].name == "Aged Brie")
                        items[i].quality  = items[i].quality + 1
                    else {
                        if (items[i].name != "Backstage passes to a TAFKAL80ETC concert") {
                            items[i].quality = items[i].quality - 1
                            if (items[i].name.startsWith("Conjured")) items[i].quality -= 1
                        }
                        else {
                            items[i].quality = items[i].quality + 1

                            if (items[i].sellIn < 11)
                                items[i].quality = items[i].quality + 1

                            if (items[i].sellIn < 6)
                                items[i].quality = items[i].quality + 1
                        }
                    }

                    items[i].sellIn = items[i].sellIn - 1

                    if (items[i].sellIn < 0)
                    {
                        if (items[i].name == "Backstage passes to a TAFKAL80ETC concert")
                                        items[i].quality = 0
                        else
                            if (items[i].name == "Aged Brie")
                                items[i].quality = items[i].quality +1
                            else {
                                items[i].quality = items[i].quality - 1
                                if (items[i].name.startsWith("Conjured")) items[i].quality -= 1
                            }
                    }

                    if (items[i].quality > 50) items[i].quality = 50;
                    if (items[i].quality < 0) items[i].quality = 0;
                }
        }
    }
}


