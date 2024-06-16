package com.platinumrose.simple

import com.gildedrose.GildedRose
import com.gildedrose.Item
import com.platinumrose.PlatinumRoseTestTemplate
import org.junit.jupiter.api.Test

class SimplePlatinumRoseTest : PlatinumRoseTestTemplate() {

    @Test
    fun `should update quality for generated test cases and compare with GlidedRose`() {
        val allTestCases = generateTestCases()
        val gildedRose = GildedRose(allTestCases.map { Item(it.name, it.sellIn, it.quality) })
        val solution = SimplePlatinumRose(allTestCases.map { Item(it.name, it.sellIn, it.quality) })
        updateQualityAndCheckResult(gildedRose, solution)
    }
}