package com.platinumrose.advance.extention

import com.gildedrose.Item
import com.platinumrose.advance.quality.QualityCalculator


fun Item.updateQuality(qualityCalculator: QualityCalculator) {
    updateQualityBeforeSellInDate(qualityCalculator)
    updateQualityAfterSellInDate(qualityCalculator)
    coerceQuality(qualityCalculator)
}

private fun Item.updateQualityBeforeSellInDate(qualityCalculator: QualityCalculator) {
    val qualityIncrease =
        qualityCalculator.computeQualityIncreaseBeforeSellIn(sellIn, quality)
    sellIn += qualityCalculator.computeSellInDecrease(sellIn)
    quality += qualityIncrease
}

private fun Item.updateQualityAfterSellInDate(qualityCalculator: QualityCalculator) {
    val qualityIncrease =
        if (sellIn >= 0) 0 else qualityCalculator.computeQualityIncreaseAfterSellIn(sellIn, quality)
    quality += qualityIncrease
}

private fun Item.coerceQuality(qualityCalculator: QualityCalculator) {
    quality = quality.coerceIn(com.platinumrose.ItemConstant.MIN_QUALITY, qualityCalculator.maxQuality())
}