package com.platinumrose.advance

import com.gildedrose.GildedRose
import com.gildedrose.Item
import com.platinumrose.PlatinumRoseTestTemplate
import org.junit.jupiter.api.Test

class AdvancePlatinumRoseTest : PlatinumRoseTestTemplate() {

    @Test
    fun `should update quality for generated test cases and compare with GlidedRose`() {
        val allTestCases = generateTestCases()
        val gildedRose = GildedRose(allTestCases.map { Item(it.name, it.sellIn, it.quality) })
        val simplePlatinumRose = AdvancePlatinumRose(allTestCases.map { Item(it.name, it.sellIn, it.quality) })
        updateQualityAndCheckResult(gildedRose, simplePlatinumRose)
    }
}