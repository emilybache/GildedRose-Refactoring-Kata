package com.gildedrose

import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test

internal class GildedRoseTest {

    @Test
    fun agedBrie_increasesInQuality() {
        val item = Item("Aged Brie", 2, 2)
        val app = GildedRose(arrayOf(item))
        app.updateQuality()
        assertThat(item.quality).isEqualTo(3)
    }

    @Test
    fun agedBrie_increasesInQualityBy2WhenSellInPasses() {
        val item = Item("Aged Brie", -2, 2)
        val app = GildedRose(arrayOf(item))
        app.updateQuality()
        assertThat(item.quality).isEqualTo(4)
    }

    @Test
    fun itemsExceptSulfurasQualityNeverNegative() {
        val items = arrayOf(
            Item("Aged Brie", 2, 0),
            Item("Backstage passes to a TAFKAL80ETC concert", 1, 0),
        )
        val app = GildedRose(items)
        app.updateQuality()
        items.forEach { item ->
            assertThat(item.quality).isGreaterThanOrEqualTo(0)
        }
    }

    @Test
    fun itemsExceptSulfurasQualityNeverGreaterThan50() {
        val items = arrayOf(
            Item("Aged Brie", 2, 50),
            Item("Backstage passes to a TAFKAL80ETC concert", 1, 50),
        )
        val app = GildedRose(items)
        app.updateQuality()
        items.forEach { item ->
            assertThat(item.quality).isLessThanOrEqualTo(50)
        }
    }

    @Test
    fun decreaseValueOfAnItemByTheEndOfADay() {
        val item = Item("Elixir of the Mongoose", 5, 7)
        val app = GildedRose(arrayOf(item))
        app.updateQuality()
        assertThat(item.quality).isEqualTo(6)
        assertThat(item.sellIn).isEqualTo(4)
    }

    @Test
    fun sulfurasNeverDecreaseQuality() {
        val item = Item("Sulfuras, Hand of Ragnaros", 5, 80)
        val app = GildedRose(arrayOf(item))
        app.updateQuality()
        assertThat(item.quality).isEqualTo(80)
    }

    @Test
    fun backStagePassesIncreasesQualityByTwoInside10Days() {
        val item = Item("Backstage passes to a TAFKAL80ETC concert", 10, 4)
        val app = GildedRose(arrayOf(item))
        app.updateQuality()
        assertThat(item.quality).isEqualTo(6)
    }

    @Test
    fun backStagePassesIncreasesQualityBy3Inside5Days() {
        val item = Item("Backstage passes to a TAFKAL80ETC concert", 5, 4)
        val app = GildedRose(arrayOf(item))
        app.updateQuality()
        assertThat(item.quality).isEqualTo(7)
    }

    @Test
    fun backStagePassesQualityGoesToZeroWhenSellInExpires() {
        val item = Item("Backstage passes to a TAFKAL80ETC concert", 0, 4)
        val app = GildedRose(arrayOf(item))
        app.updateQuality()
        assertThat(item.quality).isEqualTo(0)
        assertThat(item.sellIn).isEqualTo(-1)
    }

    @Test
    fun decreaseQualityOfAnItemByTwoWhenSellDatePasses() {
        val item = Item("Elixir of the Mongoose", -1, 7)
        val app = GildedRose(arrayOf(item))
        app.updateQuality()
        assertThat(item.quality).isEqualTo(5)
    }

}


