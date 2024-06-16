package com.platinumrose.advance.factory

import com.platinumrose.ItemType
import com.platinumrose.advance.quality.AgedBrie
import com.platinumrose.advance.quality.BackstagePasses
import com.platinumrose.advance.quality.RegularItem
import com.platinumrose.advance.quality.Sulfuras
import org.junit.jupiter.api.Assertions.assertInstanceOf
import org.junit.jupiter.api.Test

class QualityCalculatorFactoryImplTest {

    private val qualityCalculatorFactory = QualityCalculatorFactoryImpl()

    @Test
    fun `should return expected calculator for AGED_BRIE`() {
        val qualityCalculator = qualityCalculatorFactory.qualityCalculator(ItemType.AGED_BRIE)
        assertInstanceOf(AgedBrie::class.java, qualityCalculator)
    }

    @Test
    fun `should return expected calculator for BACKSTAGE_PASSES`() {
        val qualityCalculator = qualityCalculatorFactory.qualityCalculator(ItemType.BACKSTAGE_PASSES)
        assertInstanceOf(BackstagePasses::class.java, qualityCalculator)
    }

    @Test
    fun `should return expected calculator for SULFURAS`() {
        val qualityCalculator = qualityCalculatorFactory.qualityCalculator(ItemType.SULFURAS)
        assertInstanceOf(Sulfuras::class.java, qualityCalculator)
    }

    @Test
    fun `should return expected calculator for any other`() {
        val qualityCalculator = qualityCalculatorFactory.qualityCalculator(ItemType.REGULAR)
        assertInstanceOf(RegularItem::class.java, qualityCalculator)
    }
}