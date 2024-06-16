package com.platinumrose.advance.factory

import com.platinumrose.ItemType
import com.platinumrose.advance.quality.*


class QualityCalculatorFactoryImpl : QualityCalculatorFactory {
    private val strategies: Map<ItemType, QualityCalculator>
    private val regularItem = RegularItem()

    init {
        val agedBrieUpdated = AgedBrie()
        val backstagePasses = BackstagePasses()
        val sulfuras = Sulfuras()
        val strategiesList = listOf(agedBrieUpdated, backstagePasses, sulfuras)
        strategies = strategiesList.associateBy { it.type() }
    }

    override fun qualityCalculator(itemType: ItemType): QualityCalculator {
        return strategies.getOrDefault(itemType, regularItem)
    }
}