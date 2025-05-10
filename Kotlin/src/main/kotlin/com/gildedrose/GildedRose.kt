package com.gildedrose

import com.gildedrose.data.model.Item

class GildedRose(var items: List<Item>) {

    fun updateQuality() {
        for (i in items.indices) {
            if (
                items[i].name != "Aged Brie" &&
                items[i].name != "Backstage passes to a TAFKAL80ETC concert" &&
                items[i].name != "Sulfuras, Hand of Ragnaros"
            ) {
                if (items[i].quality > 0) {
                    items[i].quality -= 1
                }
            } else {
                items[i].quality = upgradeQuality(items[i].quality)

                if (items[i].name == "Backstage passes to a TAFKAL80ETC concert") {

                    if (items[i].sellIn < 11) {
                        items[i].quality = upgradeQuality(items[i].quality)
                    }

                    if (items[i].sellIn < 6) {
                        items[i].quality = upgradeQuality(items[i].quality)
                    }
                }

            }

            if (items[i].name != "Sulfuras, Hand of Ragnaros") {
                items[i].sellIn = items[i].sellIn - 1
            }

            if (items[i].sellIn < 0) {
                if (items[i].name != "Aged Brie") {
                    if (items[i].name != "Backstage passes to a TAFKAL80ETC concert") {
                        if (items[i].quality > 0) {
                            if (items[i].name != "Sulfuras, Hand of Ragnaros") {
                                items[i].quality = items[i].quality - 1
                            }
                        }
                    } else {
                        items[i].quality = items[i].quality - items[i].quality
                    }
                } else {
                    items[i].quality = upgradeQuality(items[i].quality)
                }
            }
        }
    }

    private fun upgradeQuality(quality: Int): Int {
        if (quality < 50) return quality + 1
        return quality
    }
}


