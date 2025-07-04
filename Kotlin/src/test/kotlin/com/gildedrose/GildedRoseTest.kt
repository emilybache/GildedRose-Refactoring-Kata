package com.gildedrose

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class GildedRoseTest {

    @Test
    fun sameName() {
        val items = listOf(Item("foo", 0, 0))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("foo", app.items[0].name)

    }
    @Test
    fun qualityDecreases() {
        val items = listOf(Item("foo", 1, 1))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(0, app.items[0].quality)

    }
    @Test
    fun sellinDecreases() {
        val items = listOf(Item("foo", 2, 1))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(1, app.items[0].sellIn)

    }
    @Test
    fun sellingDecreasesToNegative() {
        val items = listOf(Item("foo", 0, 1))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(-1, app.items[0].sellIn)

    }
    @Test
    fun qualityNotBelowZeroNegativeSellin() {
        val items = listOf(Item("foo", -10, 1))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(0, app.items[0].quality)

    }
    @Test
    fun qualityNotBelowZeroPosSellin() {
        val items = listOf(Item("foo", 10, 1))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(0, app.items[0].quality)

    }
    @Test
    fun qualityDegradesTwiceAsFast() {
        val items = listOf(Item("foo", -10, 10))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(8, app.items[0].quality)
    }
    @Test
    fun qualityDegradesTwiceAsFast2() {
        val items = listOf(Item("foo", 0, 10))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(8, app.items[0].quality)
    }
    @Test
    fun qualityNotMoreThan50() {
        val items = listOf(Item("foo", 0, 60))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(50, app.items[0].quality)
    }
    @Test
    fun sulfarasQualityAlway80() {
        val items = listOf(Item("Sulfuras, Hand of Ragnaros", 0, 60))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(80, app.items[0].quality)
    }

    @Test
    fun sulfarasQualityAlway80Again() {
        val items = listOf(Item("Sulfuras, Hand of Ragnaros", 0, 30))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(80, app.items[0].quality)
    }
    @Test
    fun agedBrieIncreases() {
        val items = listOf(Item("Aged Brie", 10, 30))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(31, app.items[0].quality)
    }
    @Test
    fun agedBrieDoesNotIncreaseOver50() {
        val items = listOf(Item("Aged Brie", 10, 50))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(50, app.items[0].quality)
    }
    @Test
    fun agedBrieDoesNotIncreaseOver50AfterSellin() {
        val items = listOf(Item("Aged Brie", 0, 49))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(50, app.items[0].quality)
    }

    @Test
    fun agedBrieDoesNotIncreaseTwiceFasterAfterSellin() {
        val items = listOf(Item("Aged Brie", 0, 40))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(42, app.items[0].quality)
    }


    @Test
    fun agedBrieNotBelowZero() {
        val items = listOf(Item("Aged Brie", 0, 49))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(50, app.items[0].quality)
    }

    @Test
    fun concertZeroAfterSellin() {
        val items = listOf(Item("Backstage passes to a TAFKAL80ETC concert", 0, 49))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(0, app.items[0].quality)
    }
    @Test
    fun concertIncreaseBefore10DaysBeforeSellin() {
        val items = listOf(Item("Backstage passes to a TAFKAL80ETC concert", 20, 40))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(41, app.items[0].quality)
    }
    @Test
    fun concertIncreasesTwice10to5DaysBeforeSellin() {
        val items = listOf(Item("Backstage passes to a TAFKAL80ETC concert", 10, 40))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(42, app.items[0].quality)
    }

    @Test
    fun concertIncreasesThrice5DaysBeforeSellin() {
        val items = listOf(Item("Backstage passes to a TAFKAL80ETC concert", 5, 40))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(43, app.items[0].quality)
    }

    @Test
    fun conjuredItemsBeforeSellin() {
        val items = listOf(Item("Conjured Mnan cake", 5, 40))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(38, app.items[0].quality)
    }

    @Test
    fun conjuredItemsAfterSellin() {
        val items = listOf(Item("Conjured Mana Cake", 0, 40))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals(36, app.items[0].quality)
    }
}


