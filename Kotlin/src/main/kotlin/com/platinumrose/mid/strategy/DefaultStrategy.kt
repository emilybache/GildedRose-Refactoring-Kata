package com.platinumrose.mid.strategy

import com.gildedrose.Item
import com.platinumrose.ItemConstant.Companion.REGULAR_ITEM_MAX_QUALITY
import com.platinumrose.ItemType.REGULAR


internal class DefaultStrategy : UpdateQualityStrategy {

    override fun type(): com.platinumrose.ItemType {
        return REGULAR
    }

    override fun updateQuality(item: Item) {
        item.quality -= 1
        item.sellIn -= 1
        if (item.sellIn < 0) {
            item.quality -= 1
        }
    }

    override fun maxQuality(): Int {
        return REGULAR_ITEM_MAX_QUALITY
    }
}