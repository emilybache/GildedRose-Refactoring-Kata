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
                items[i].quality = downGradeQuality(items[i].quality)
            } else {
                items[i].quality = upgradeQuality(items[i].quality)

                if (items[i].name == "Backstage passes to a TAFKAL80ETC concert") {
                    when {
                        items[i].sellIn < 6 -> {
                            items[i].quality = upgradeQuality(items[i].quality)
                            items[i].quality = upgradeQuality(items[i].quality)
                        }

                        items[i].sellIn < 11 -> {
                            items[i].quality = upgradeQuality(items[i].quality)
                        }
                    }
                }
            }

            if (items[i].name != "Sulfuras, Hand of Ragnaros") {
                items[i].sellIn = items[i].sellIn - 1
            }

            if (items[i].sellIn < 0) {
                when (items[i].name) {
                    "Aged Brie" -> {
                        items[i].quality = upgradeQuality(items[i].quality)
                    }

                    "Backstage passes to a TAFKAL80ETC concert" -> {
                        items[i].quality -= items[i].quality
                    }

                    else -> {
                        if (items[i].name != "Sulfuras, Hand of Ragnaros") {
                            items[i].quality = downGradeQuality(items[i].quality)
                        }
                    }
                }
            }
        }
    }

    private fun downGradeQuality(quality: Int): Int {
        if (quality > 0) return quality - 1
        return quality
    }

    private fun upgradeQuality(quality: Int): Int {
        if (quality < 50) return quality + 1
        return quality
    }
}


