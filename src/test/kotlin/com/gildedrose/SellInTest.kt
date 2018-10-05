package com.gildedrose

import com.gildedrose.core.TestUtils
import com.gildedrose.core.TestUtils.generateRandomName
import com.gildedrose.core.TestUtils.generateRandomNumber
import com.gildedrose.core.TestUtils.pickRandomItem
import com.gildedrose.core.advanceTimeBy
import org.assertj.core.api.Java6Assertions.assertThat
import org.junit.Before
import org.junit.Test

class SellInTest {

    private lateinit var store: List<Item>

    @Before
    fun setUp() {
        store = TestUtils.fixture.toList()
    }

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
    fun givenKnownItemExceptSulfuras_afterRandomNumberOfDays_shouldDecreaseSellInByNumberOfDays() {
        store = store.filter { it.name != "Sulfuras, Hand of Ragnaros" }
        val days = generateRandomNumber()
        val initialSellIn = generateRandomNumber()
        val item = pickRandomItem(store).copy(sellIn = initialSellIn)

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
}
