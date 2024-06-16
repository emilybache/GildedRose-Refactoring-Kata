package com.platinumrose.mid

import com.gildedrose.Item
import com.platinumrose.ItemConstant.Companion.MIN_QUALITY
import com.platinumrose.Solution
import com.platinumrose.mid.strategy.*


class MidPlatinumRose(var items: List<Item>) : Solution {
    private val strategies: Map<com.platinumrose.ItemType, UpdateQualityStrategy>
    private val defaultStrategy = DefaultStrategy()

    init {
        val agedBrieStrategy = AgedBrieStrategy()
        val backstagePassesStrategy = BackstagePassesStrategy()
        val sulfurasStrategy = SulfurasStrategy()
        val strategiesList = listOf(agedBrieStrategy, backstagePassesStrategy, sulfurasStrategy)
        strategies = strategiesList.associateBy { it.type() }
    }

    override fun items(): List<Item> {
        return items
    }

    override fun updateQuality() {
        for (item in items) {
            val strategy = findUpdateQualityStrategy(item)
            strategy.updateQuality(item)
            item.quality = item.quality.coerceIn(MIN_QUALITY, strategy.maxQuality())
        }
    }

    private fun findUpdateQualityStrategy(item: Item): UpdateQualityStrategy {
        return strategies.getOrDefault(com.platinumrose.ItemType.fromValue(item.name), defaultStrategy)
    }
}