package com.platinumrose.mid.strategy

import com.gildedrose.Item
import com.platinumrose.ItemConstant.Companion.REGULAR_ITEM_MAX_QUALITY
import com.platinumrose.ItemType.BACKSTAGE_PASSES


internal class BackstagePassesStrategy : UpdateQualityStrategy {

    override fun type(): com.platinumrose.ItemType {
        return BACKSTAGE_PASSES
    }

    override fun updateQuality(item: Item) {
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

    override fun maxQuality(): Int {
        return REGULAR_ITEM_MAX_QUALITY
    }
}