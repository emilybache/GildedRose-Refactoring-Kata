package com.platinumrose.simple

import com.gildedrose.Item
import com.platinumrose.Solution

class SimplePlatinumRose(var items: List<Item>) : Solution {

    companion object {
        const val MIN_QUALITY = 0
        const val REGULAR_ITEM_MAX_QUALITY = 50
        const val LEGENDARY_ITEM_MAX_QUALITY = 80

        const val AGED_BRIE = "Aged Brie"
        const val BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert"
        const val LULFURAS_HAND_OF_RAGNAROK = "Sulfuras, Hand of Ragnaros"
    }


    override fun items(): List<Item> {
        return items
    }


    override fun updateQuality() {
        for (item in items) {
            when (item.name) {
                com.platinumrose.ItemType.AGED_BRIE.value -> updateQualityForAgedBrie(item)
                com.platinumrose.ItemType.BACKSTAGE_PASSES.value -> updateQualityForBackstagePasses(item)
                com.platinumrose.ItemType.SULFURAS.value -> updateQualityForSulfuras(item)
                else -> updateQuality(item)
            }
        }
    }

    private fun updateQuality(item: Item) {
        item.quality -= 1
        item.sellIn -= 1
        if (item.sellIn < 0) {
            item.quality -= 1
        }
        item.quality = item.quality.coerceIn(0, REGULAR_ITEM_MAX_QUALITY)
    }

    private fun updateQualityForAgedBrie(item: Item) {
        item.quality += 1
        item.sellIn -= 1
        if (item.sellIn < 0) {
            item.quality += 1
        }
        item.quality = item.quality.coerceIn(0, REGULAR_ITEM_MAX_QUALITY)
    }

    private fun updateQualityForBackstagePasses(item: Item) {
        item.quality += 1
        if (item.sellIn < 11) {
            item.quality += 1
        }
        if (item.sellIn < 6) {
            item.quality += 1
        }
        item.sellIn -= 1
        if (item.sellIn < 0) {
            item.quality = 0
        }
        item.quality = item.quality.coerceIn(0, REGULAR_ITEM_MAX_QUALITY)
    }

    private fun updateQualityForSulfuras(item: Item) {
        item.quality = item.quality.coerceIn(0, LEGENDARY_ITEM_MAX_QUALITY)
    }
}