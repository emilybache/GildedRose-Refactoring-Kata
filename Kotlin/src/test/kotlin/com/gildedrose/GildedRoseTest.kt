package com.gildedrose

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class GildedRoseTest {

    @Test
    fun dexterityVestNormal() {
        val items = listOf(Item("+5 Dexterity Vest", 10, 20))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("+5 Dexterity Vest",  app.items[0].name)
        assertEquals(9, app.items[0].sellIn)
        assertEquals(19, app.items[0].quality)
    }

    @Test
    fun dexterityVestNegativeSellIn() {
        val items = listOf(Item("+5 Dexterity Vest", 0, 20))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("+5 Dexterity Vest",  app.items[0].name)
        assertEquals(-1, app.items[0].sellIn)
        assertEquals(18, app.items[0].quality)
    }

    @Test
    fun agedBrieNormal() {
        val items = listOf(Item("Aged Brie", 2, 0))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Aged Brie",  app.items[0].name)
        assertEquals(1, app.items[0].sellIn)
        assertEquals(1, app.items[0].quality)
    }

    @Test
    fun elixirOfTheMongooseNormal() {
        val items = listOf(Item("Elixir of the Mongoose", 5, 7))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Elixir of the Mongoose",  app.items[0].name)
        assertEquals(4, app.items[0].sellIn)
        assertEquals(6, app.items[0].quality)
    }

    @Test
    fun elixirOfTheMongooseNegativeSellIn() {
        val items = listOf(Item("Elixir of the Mongoose", 0, 7))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Elixir of the Mongoose",  app.items[0].name)
        assertEquals(-1, app.items[0].sellIn)
        assertEquals(5, app.items[0].quality)
    }


    @Test
    fun sulfurasAt0() {
        val items = listOf(Item("Sulfuras, Hand of Ragnaros", 0, 80))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Sulfuras, Hand of Ragnaros",  app.items[0].name)
        assertEquals(0, app.items[0].sellIn)
        assertEquals(80, app.items[0].quality)
    }

    @Test
    fun sulfurasAtMinus1() {
        val items = listOf(Item("Sulfuras, Hand of Ragnaros", -1, 80))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Sulfuras, Hand of Ragnaros",  app.items[0].name)
        assertEquals(-1, app.items[0].sellIn)
        assertEquals(80, app.items[0].quality)
    }

    @Test
    fun  backstagePassesMoreThan10() {
        val items = listOf(Item("Backstage passes to a TAFKAL80ETC concert", 15, 20))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Backstage passes to a TAFKAL80ETC concert",  app.items[0].name)
        assertEquals(14, app.items[0].sellIn)
        assertEquals(21, app.items[0].quality)
    }

    @Test
    fun  backstagePassesReached50at10() {
        val items = listOf(Item("Backstage passes to a TAFKAL80ETC concert", 10, 49))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Backstage passes to a TAFKAL80ETC concert",  app.items[0].name)
        assertEquals(9, app.items[0].sellIn)
        assertEquals(50, app.items[0].quality)
    }

    @Test
    fun  backstagePassesReached50at5() {
        val items = listOf(Item("Backstage passes to a TAFKAL80ETC concert", 5, 49))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Backstage passes to a TAFKAL80ETC concert",  app.items[0].name)
        assertEquals(4, app.items[0].sellIn)
        assertEquals(50, app.items[0].quality)
    }

    @Test
    fun  backstagePassesAt10() {
        val items = listOf(Item("Backstage passes to a TAFKAL80ETC concert", 10, 30))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Backstage passes to a TAFKAL80ETC concert",  app.items[0].name)
        assertEquals(9, app.items[0].sellIn)
        assertEquals(32, app.items[0].quality)
    }


    @Test
    fun  backstagePassesAt5() {
        val items = listOf(Item("Backstage passes to a TAFKAL80ETC concert", 5, 30))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Backstage passes to a TAFKAL80ETC concert",  app.items[0].name)
        assertEquals(4, app.items[0].sellIn)
        assertEquals(33, app.items[0].quality)
    }

    @Test
    fun  backstagePassesAt0() {
        val items = listOf(Item("Backstage passes to a TAFKAL80ETC concert", 0, 30))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Backstage passes to a TAFKAL80ETC concert",  app.items[0].name)
        assertEquals(-1, app.items[0].sellIn)
        assertEquals(0, app.items[0].quality)
    }




    @Test
    fun  conjuredBasic() {
        val items = listOf(Item("Conjured Mana Cake", 3, 6))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Conjured Mana Cake",  app.items[0].name)
        assertEquals(2, app.items[0].sellIn)
        assertEquals(4, app.items[0].quality)
    }

    @Test
    fun  conjuredBasicNegativeSellIn() {
        val items = listOf(Item("Conjured Mana Cake", 0, 6))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("Conjured Mana Cake",  app.items[0].name)
        assertEquals(-1, app.items[0].sellIn)
        assertEquals(2, app.items[0].quality)
    }




}
