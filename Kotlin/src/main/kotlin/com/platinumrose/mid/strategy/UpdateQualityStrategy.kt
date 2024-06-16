package com.platinumrose.mid.strategy

import com.gildedrose.Item

interface UpdateQualityStrategy {
    fun type(): com.platinumrose.ItemType
    fun updateQuality(item: Item)
    fun maxQuality(): Int
}