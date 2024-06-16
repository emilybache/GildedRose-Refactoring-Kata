package com.platinumrose.mid.strategy

import com.gildedrose.Item
import com.platinumrose.ItemConstant.Companion.LEGENDARY_ITEM_MAX_QUALITY
import com.platinumrose.ItemType.SULFURAS


internal class SulfurasStrategy : UpdateQualityStrategy {

    override fun type(): com.platinumrose.ItemType {
        return SULFURAS
    }

    override fun updateQuality(item: Item) {
        // nothing to do here
    }

    override fun maxQuality(): Int {
        return LEGENDARY_ITEM_MAX_QUALITY
    }
}


