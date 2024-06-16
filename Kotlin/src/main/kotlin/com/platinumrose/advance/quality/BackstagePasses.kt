package com.platinumrose.advance.quality

import com.platinumrose.ItemConstant.Companion.REGULAR_ITEM_MAX_QUALITY
import com.platinumrose.ItemType.BACKSTAGE_PASSES


internal class BackstagePasses : QualityCalculator {

    override fun type(): com.platinumrose.ItemType {
        return BACKSTAGE_PASSES
    }

    override fun maxQuality(): Int {
        return REGULAR_ITEM_MAX_QUALITY
    }

    override fun computeSellInDecrease(sellIn: Int): Int {
        return -1
    }

    override fun computeQualityIncreaseBeforeSellIn(sellIn: Int, quality: Int): Int {
        return when (sellIn) {
            in 0..5 -> 3
            in 6..10 -> 2
            else -> 1
        }
    }

    override fun computeQualityIncreaseAfterSellIn(sellIn: Int, quality: Int): Int {
        return -quality
    }
}