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
                downGradeQuality(i)
            } else {
                upgradeQuality(i)

                if (items[i].name == "Backstage passes to a TAFKAL80ETC concert") {
                    when {
                        items[i].sellIn < 6 -> {
                            upgradeQuality(i)
                            upgradeQuality(i)
                        }

                        items[i].sellIn < 11 -> {
                            upgradeQuality(i)
                        }
                    }
                }
            }

            sellItem(i)

            if (items[i].sellIn < 0) {
                when (items[i].name) {
                    "Aged Brie" -> {
                        upgradeQuality(i)
                    }

                    "Backstage passes to a TAFKAL80ETC concert" -> {
                        items[i].quality -= items[i].quality
                    }

                    else -> {
                        if (items[i].name != "Sulfuras, Hand of Ragnaros") {
                            downGradeQuality(i)
                        }
                    }
                }
            }
        }
    }

    private fun sellItem(i: Int) {
        if (items[i].name != "Sulfuras, Hand of Ragnaros") items[i].sellIn -= 1
    }

    private fun downGradeQuality(i: Int) {
        if (items[i].quality > 0) items[i].quality -= 1
    }

    private fun upgradeQuality(i: Int) {
        if (items[i].quality < 50) items[i].quality += 1

    }
}


