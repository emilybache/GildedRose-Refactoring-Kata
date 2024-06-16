package com.platinumrose

enum class ItemType(val value: String? = null) {
    AGED_BRIE("Aged Brie"),
    BACKSTAGE_PASSES("Backstage passes to a TAFKAL80ETC concert"),
    SULFURAS("Sulfuras, Hand of Ragnaros"),
    REGULAR;

    companion object {
        fun fromValue(value: String): com.platinumrose.ItemType {
            return entries.find { it.value == value } ?: com.platinumrose.ItemType.REGULAR
        }
    }
}