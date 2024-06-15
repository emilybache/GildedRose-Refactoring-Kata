package com.gildedrose

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.util.*

class PlatinumRoseTest {

    @Test
    fun `should update quality for generated test cases and compare with GlidedRose`() {
        val names = listOf(
            "Aged Brie",
            "Backstage passes to a TAFKAL80ETC concert",
            "Sulfuras, Hand of Ragnaros",
            "new none-existing on code name"
        )
        val sellInRange = -100..100
        val qualityRange = -100..100
        val allTestCases = generateTestCasesInRanger(names, sellInRange, qualityRange)


        for (testCase in allTestCases) {
            val platinumRose = PlatinumRose(listOf(Item(testCase.name, testCase.sellIn, testCase.quality)))
            val gildedRose = GildedRose(listOf(Item(testCase.name, testCase.sellIn, testCase.quality)))

            platinumRose.updateQuality()
            gildedRose.updateQuality()

            assertEquals(gildedRose.items, platinumRose.items)
        }
    }

    private fun generateTestCasesInRanger(
        names: List<String>,
        sellInRange: IntRange,
        qualityRange: IntRange
    ): List<Item> {
        val items = LinkedList<Item>()
        for (name in names) {
            for (sellIn in sellInRange) {
                for (quality in qualityRange) {
                    items.add(Item(name, sellIn, quality))
                }
            }
        }
        return items
    }
}