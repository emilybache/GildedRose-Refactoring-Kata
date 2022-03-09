package com.gildedrose

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

internal class GildedRoseTest {

    @Test
    fun foo() {
        val items = arrayOf<Item>(Item("foo", 0, 0))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("foo", app.items[0].name)

    }

    @Test
    fun `decreaseNormalQualityTest`() {
        val items = arrayOf<Item>(Item("Elixir of the Mongoose", 5, 7),)
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(6, app.items[0].quality)
    }

    @Test
    fun decreaseConjuredQualityTest() {
        val items = arrayOf(Item("Conjured Mana Cake", 1, 6))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(4, app.items[0].quality)
    }

}


