package com.gildedrose

import com.gildedrose.core.TestUtils
import com.gildedrose.core.TestUtils.generateRandomNumber
import com.gildedrose.core.TestUtils.pickRandomItem
import com.gildedrose.core.advanceTimeBy
import org.assertj.core.api.Java6Assertions.assertThat
import org.junit.Before
import org.junit.Test

class QualityTest {

    private lateinit var store: List<Item>

    @Before
    fun setUp() {
        store = TestUtils.fixture.toList()
    }

    @Test
    fun givenKnownItemExcepSulfuras_afterAnyNumberOfDays_shouldHaveQualityGreaterThanOrEqualToZero() {
        val days = generateRandomNumber()
        store = store.filter { it.name != "Sulfuras, Hand of Ragnaros" }
        val app = GildedRose(store.toTypedArray())

        app.advanceTimeBy(days)

        store.forEach {item ->
            assertThat(item.quality).isGreaterThanOrEqualTo(0)
        }
    }

    @Test
    fun givenKnownItemExcepSulfuras_afterAnyNumberOfDays_shouldHaveQualityLessThanOrEqualToFifty() {
        val days = generateRandomNumber()
        store = store.filter { it.name != "Sulfuras, Hand of Ragnaros" }
        val app = GildedRose(store.toTypedArray())

        app.advanceTimeBy(days)

        store.forEach {item ->
            assertThat(item.quality).isLessThanOrEqualTo(50)
        }
    }
}
