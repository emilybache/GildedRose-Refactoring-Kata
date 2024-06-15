package com.gildedrose

import com.gildedrose.PlatinumRose.Companion.AGED_BRIE
import com.gildedrose.PlatinumRose.Companion.BACKSTAGE_PASSES
import com.gildedrose.PlatinumRose.Companion.LULFURAS_HAND_OF_RAGNAROK
import com.gildedrose.PlatinumRose.Companion.MIN_QUALITY
import com.gildedrose.PlatinumRose.Companion.REGULAR_ITEM_MAX_QUALITY
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.util.*

class PlatinumRoseTest {

    @Test
    fun `should update quality for generated test cases and compare with GlidedRose`() {
        val names = listOf(
            AGED_BRIE,
            BACKSTAGE_PASSES,
            LULFURAS_HAND_OF_RAGNAROK,
            "new none-existing on code name"
        )
        val sellInRange = -100..100
        val qualityRange = MIN_QUALITY..REGULAR_ITEM_MAX_QUALITY
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