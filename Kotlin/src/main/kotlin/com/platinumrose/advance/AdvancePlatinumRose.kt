package com.platinumrose.advance

import com.gildedrose.Item
import com.platinumrose.ItemType.Companion.fromValue
import com.platinumrose.Solution
import com.platinumrose.advance.extention.updateQuality
import com.platinumrose.advance.factory.QualityCalculatorFactory
import com.platinumrose.advance.factory.QualityCalculatorFactoryImpl

class AdvancePlatinumRose(private val items: List<Item>) : Solution {

    private val qualityCalculatorFactory: QualityCalculatorFactory = QualityCalculatorFactoryImpl()

    override fun items(): List<Item> {
        return items
    }

    override fun updateQuality() {
        items.forEach {
            it.updateQuality(qualityCalculatorFactory.qualityCalculator(fromValue(it.name)))
        }
    }
}
