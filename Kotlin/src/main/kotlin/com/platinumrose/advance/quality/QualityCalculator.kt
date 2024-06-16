package com.platinumrose.advance.quality

interface QualityCalculator {

    fun type(): com.platinumrose.ItemType
    fun maxQuality(): Int

    fun computeQualityIncreaseBeforeSellIn(sellIn: Int, quality: Int): Int;
    fun computeSellInDecrease(sellIn: Int): Int
    fun computeQualityIncreaseAfterSellIn(sellIn: Int, quality: Int): Int;
}