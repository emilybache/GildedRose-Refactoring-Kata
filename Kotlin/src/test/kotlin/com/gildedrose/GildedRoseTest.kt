package com.gildedrose

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class GildedRoseTest {

    private val gildedRose = GildedRose(
        arrayOf(
            Item("+5 Dexterity Vest", 10, 20), //
            Item("Aged Brie", 2, 0), //
            Item("Elixir of the Mongoose", 5, 7), //
            Item("Sulfuras, Hand of Ragnaros", 0, 80), //
            Item("Sulfuras, Hand of Ragnaros", -1, 80),
            Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
            // this conjured item does not work properly yet
            Item("Conjured Mana Cake", 3, 6)
        )
    )

    @Test
    fun allItemsHaveASellInValue() {
        gildedRose.items.forEach {
            assertNotNull(it.sellIn)
        }

    }

    @Test
    fun qualityIsNeverNegative() {
        (gildedRose.items.map { it.sellIn }.maxOrNull()!! downTo -3).forEach {
            gildedRose.updateQuality()
            gildedRose.items.forEach {
                assertTrue(it.quality >= 0, "The quality of an item is never negative")
            }
        }
    }

    @Test
    fun qualityIsNeverMoreThanFifty() {
        val notSulfuras = gildedRose.items.filterNot { it.name.startsWith("Sulfuras") }
        (gildedRose.items.map { it.sellIn }.maxOrNull()!! downTo -3).forEach {
            gildedRose.updateQuality()
            assertFalse(notSulfuras.map { it.quality }.maxOrNull()!! > 50)
        }
    }

    @Test
    fun ordinaryItemsDecreaseInSellIn() {
        val elixir = gildedRose.items.first { it.name.startsWith("Elixir") }
        while (elixir.sellIn >= 0) {
            val sellIn = elixir.sellIn
            gildedRose.updateQuality()
            assertEquals(sellIn - 1, elixir.sellIn)
        }
    }

    @Test
    fun ordinaryItemsDecreaseInQualityUntilZero() {
        val elixir = gildedRose.items.first { it.name.startsWith("Elixir") }
        while (elixir.quality != 0) {
            val quality = elixir.quality
            gildedRose.updateQuality()
            assertTrue(elixir.quality < quality)
        }
        gildedRose.updateQuality()
        assertEquals(0, elixir.quality)
    }

    @Test
    fun afterSellByDateHasPassedQualityDecreasesTwiceAsFase() {
        val elixir = gildedRose.items.first { it.name.startsWith("Elixir") }
        while (elixir.sellIn > 0) {
            val quality = elixir.quality
            gildedRose.updateQuality()
            assertEquals(quality - 1, elixir.quality)
        }
        val quality = elixir.quality
        gildedRose.updateQuality()
        assertEquals(quality - 2, elixir.quality)
        while (elixir.sellIn > -3) {
            gildedRose.updateQuality()
            assertEquals(0, elixir.quality)
        }
    }

    @Test
    fun agedBrieIncreasesInQuality() {
        gildedRose.items.first { it.name == "Aged Brie" }.also { agedBrie ->
            while (agedBrie.quality < 50) {
                val quality = agedBrie.quality
                gildedRose.updateQuality()
                assertTrue(agedBrie.quality > quality)
            }
            gildedRose.updateQuality()
            assertTrue(agedBrie.quality == 50)
        }
    }

    @Test
    fun sulfurasNeverHasToBeSoldOrDecreasesInQuality() {
        val sulfuras = gildedRose.items.first { it.name.startsWith("Sulfuras") }
        val quality = sulfuras.quality
        val sellIn = sulfuras.sellIn
        gildedRose.updateQuality()
        assertEquals(quality, sulfuras.quality)
        assertEquals(sellIn, sulfuras.sellIn)
    }

    @Test
    fun backstagePassesAreDifferent() {
        val backstage = gildedRose.items.first { it.name.startsWith("Backstage") }
        while (backstage.sellIn > 10) {
            val quality = backstage.quality
            gildedRose.updateQuality()
            assertEquals(quality + 1, backstage.quality)
        }
        while (backstage.sellIn > 5) {
            val quality = backstage.quality
            gildedRose.updateQuality()
            assertEquals(quality + 2, backstage.quality)
        }
        while (backstage.sellIn > 0) {
            val quality = backstage.quality
            gildedRose.updateQuality()
            assertEquals(quality + 3, backstage.quality)
        }
        gildedRose.updateQuality()
        assertEquals(-1, backstage.sellIn)
        assertEquals(0, backstage.quality)
    }
}
