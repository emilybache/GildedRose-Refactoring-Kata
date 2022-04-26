package com.gildedrose

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class GildedRoseTest {

    val items = arrayOf(
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

    @Test
    fun foo() {
        val items = arrayOf<Item>(Item("foo", 0, 0))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("foo", app.items[0].name)

    }

    @Test
    fun allItemsHaveASellInValue() {
        items.forEach {
            assertNotNull(it.sellIn)
        }

    }

    private val gildedRose = GildedRose(items)

    @Test
    fun qualityIsNeverNegative() {
        (1..100).forEach {
            gildedRose.updateQuality()
            items.forEach {
                assertTrue(it.quality >= 0, "The quality of an item is never negative")
            }
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
    fun agedBrieIncreasesInQuality() {
        items.first { it.name == "Aged Brie" }.also { agedBrie ->
            while (agedBrie.quality < 50) {
                val quality = agedBrie.quality
                gildedRose.updateQuality()
                assertTrue(agedBrie.quality > quality)
            }
        }
    }

    @Test
    fun sulfurasNeverHasToBeSoldOrDecreasesInQuality() {
        val sulfuras = items.first { it.name.startsWith("Sulfuras") }
        val quality = sulfuras.quality
        val sellIn = sulfuras.sellIn
        gildedRose.updateQuality()
        assertEquals(quality, sulfuras.quality)
        assertEquals(sellIn, sulfuras.sellIn)
    }

}


