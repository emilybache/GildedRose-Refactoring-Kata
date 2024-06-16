package com.platinumrose.advance.factory

import com.platinumrose.ItemType
import com.platinumrose.advance.quality.QualityCalculator


interface QualityCalculatorFactory {
    fun qualityCalculator(itemType: ItemType): QualityCalculator
}