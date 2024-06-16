package com.platinumrose.advance.quality

import com.platinumrose.ItemConstant.Companion.LEGENDARY_ITEM_MAX_QUALITY
import com.platinumrose.ItemType.SULFURAS


internal class Sulfuras : QualityCalculator {

    override fun type(): com.platinumrose.ItemType {
        return SULFURAS
    }

    override fun maxQuality(): Int {
        return LEGENDARY_ITEM_MAX_QUALITY
    }

    override fun computeSellInDecrease(sellIn: Int): Int {
        return 0
    }

    override fun computeQualityIncreaseBeforeSellIn(sellIn: Int, quality: Int): Int {
        return 0
    }

    override fun computeQualityIncreaseAfterSellIn(sellIn: Int, quality: Int): Int {
        return 0
    }
}
