package com.platinumrose

import com.gildedrose.GildedRose
import com.gildedrose.Item
import com.platinumrose.ItemConstant.Companion.MIN_QUALITY
import com.platinumrose.ItemConstant.Companion.REGULAR_ITEM_MAX_QUALITY
import org.junit.jupiter.api.Assertions.assertEquals
import java.util.*

abstract class PlatinumRoseTestTemplate {
    private val ITEMS_NAMES = listOf(
        com.platinumrose.ItemType.AGED_BRIE.value!!,
        com.platinumrose.ItemType.BACKSTAGE_PASSES.value!!,
        com.platinumrose.ItemType.SULFURAS.value!!,
        "new none-existing on code name"
    )
    private val SELLIN_RANGE = -100..100
    private val QUALITY_RANGE = MIN_QUALITY..REGULAR_ITEM_MAX_QUALITY


    fun generateTestCases(): List<Item> {
        return generateTestCasesInRanger(ITEMS_NAMES, SELLIN_RANGE, QUALITY_RANGE)
    }

    fun updateQualityAndCheckResult(gildedRose: GildedRose, simplePlatinumRose: Solution) {
        simplePlatinumRose.updateQuality()
        gildedRose.updateQuality()
        assertEquals(gildedRose.items, simplePlatinumRose.items())
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