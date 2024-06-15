package com.gildedrose

class PlatinumRose(var items: List<Item>) {

    companion object {
        const val MIN_QUALITY = 0
        const val REGULAR_ITEM_MAX_QUALITY = 50
        const val LEGENDARY_ITEM_MAX_QUALITY = 80

        const val AGED_BRIE = "Aged Brie"
        const val BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert"
        const val LULFURAS_HAND_OF_RAGNAROK = "Sulfuras, Hand of Ragnaros"
    }

    fun updateQuality() {
        for (item in items) {
            when (item.name) {
                AGED_BRIE -> updateQualityForAgedBrie(item)
                BACKSTAGE_PASSES -> updateQualityForBackstagePasses(item)
                LULFURAS_HAND_OF_RAGNAROK -> null
                else -> updateQuality(item)
            }
            item.quality = item.quality.coerceIn(0, REGULAR_ITEM_MAX_QUALITY)
        }
    }

    private fun updateQuality(item: Item) {
        item.quality -= 1
        item.sellIn -= 1
        if (item.sellIn < 0) {
            item.quality -= 1
        }
    }

    private fun updateQualityForAgedBrie(item: Item) {
        item.quality += 1
        item.sellIn -= 1
        if (item.sellIn < 0) {
            item.quality += 1
        }
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
    }
}