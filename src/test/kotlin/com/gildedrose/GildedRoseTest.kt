package com.gildedrose

import org.assertj.core.api.Java6Assertions.assertThat
import org.junit.Test
import java.util.*

class GildedRoseTest {

    private val store = arrayOf(
            Item("+5 Dexterity Vest", 10, 20), //
            Item("Aged Brie", 2, 0), //
            Item("Elixir of the Mongoose", 5, 7), //
            Item("Sulfuras, Hand of Ragnaros", 0, 80), //
            Item("Sulfuras, Hand of Ragnaros", -1, 80),
            Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            Item("Backstage passes to a TAFKAL80ETC concert", 5, 49)
    )

    @Test
    fun givenUnknownItem_afterRandomNumberOfDays_shouldDecreaseSellInByNumberOfDays() {
        val days = generateRandomNumber()
        val initialSellIn = generateRandomNumber()
        val item = Item(generateRandomName(), initialSellIn, 0)
        val app = GildedRose(arrayOf(item))

        app.advanceTimeBy(days)

        assertThat(item.sellIn).isEqualTo(initialSellIn - days)
    }

    @Test
    fun givenKnownItemExcepSulfuras_afterRandomNumberOfDays_shouldDecreaseSellInByNumberOfDays() {
        val days = generateRandomNumber()
        val initialSellIn = generateRandomNumber()
        var item = pickRandomItem().copy(sellIn = initialSellIn)

        while (item.name == "Sulfuras, Hand of Ragnaros") {
            item = pickRandomItem().copy(sellIn = initialSellIn)
        }

        val app = GildedRose(arrayOf(item))

        app.advanceTimeBy(days)

        assertThat(item.sellIn).isEqualTo(initialSellIn - days)
    }

    @Test
    fun givenSulfuras_afterRandomNumberOfDays_shouldHaveSameSellIn() {
        val days = generateRandomNumber()
        val initialSellIn = generateRandomNumber()
        val item = Item("Sulfuras, Hand of Ragnaros", initialSellIn, 0)
        val app = GildedRose(arrayOf(item))

        app.advanceTimeBy(days)

        assertThat(item.sellIn).isEqualTo(initialSellIn)
    }

    private fun GildedRose.advanceTimeBy(days: Int) = repeat(days, { this.updateQuality() })

    private fun generateRandomName() = UUID.randomUUID().toString()

    private fun generateRandomNumber() = (0 until 9999).shuffled().last()

    private fun pickRandomItem() = store[generateRandomNumber().coerceIn(store.indices)]
}
