package com.gildedrose

import com.gildedrose.data.model.Item

class GildedRose(var items: List<Item>) {

    fun updateQuality() {
        for (i in items.indices) {
            when (items[i].name) {
                "Backstage passes to a TAFKAL80ETC concert" -> {
                    updateBackstagePassesQuality(i)
                }

                "Aged Brie" -> {
                    updateAgedBrieQuality(i)
                }

                "Sulfuras, Hand of Ragnaros" -> {
                    updateSulfurasQuality(i)
                }

                else -> {
                    downGradeQuality(i)
                    if (items[i].sellIn < 1) downGradeQuality(i)
                }
            }

            if (items[i].name != "Sulfuras, Hand of Ragnaros") sellItem(i)
        }
    }

    private fun updateBackstagePassesQuality(i: Int) {
        upgradeQuality(i)
        when {
            items[i].sellIn < 1 -> {
                items[i].quality -= items[i].quality
            }

            items[i].sellIn < 6 -> {
                upgradeQuality(i)
                upgradeQuality(i)
            }

            items[i].sellIn < 11 -> {
                upgradeQuality(i)
            }
        }
    }

    private fun updateAgedBrieQuality(i: Int) {
        upgradeQuality(i)
        if (items[i].sellIn < 1) {
            upgradeQuality(i)
        }
    }

    private fun updateSulfurasQuality(i: Int) {
        upgradeQuality(i)
    }

    private fun sellItem(i: Int) {
        items[i].sellIn -= 1
    }

    private fun downGradeQuality(i: Int) {
        if (items[i].quality > 0) items[i].quality -= 1
    }

    private fun upgradeQuality(i: Int) {
        if (items[i].quality < 50) items[i].quality += 1
    }
}


