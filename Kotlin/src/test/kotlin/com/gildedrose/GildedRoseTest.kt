package com.gildedrose

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class GildedRoseTest {

    @Test
    internal fun `regular item decreases as expected`() {
        val items = arrayOf(Item("foo", 1, 1))
        val app = GildedRose(items)
        app.updateQuality()

        app.assertQualityAndSellIn(
                expectedSellIn = 0,
                expectedQuality = 0
        )
    }

    @Test
    internal fun `Quality decreases twice as fast after expiry date passed`() {
        val item = Item("foo", 0, 2)
        val app = GildedRose(arrayOf(item))
        app.updateQuality()

        app.assertQualityAndSellIn(
                expectedSellIn = -1,
                expectedQuality = 0
        )
    }

    @Test
    internal fun `Quality decreases normally when reaching sellIn date`() {
        val item = Item("foo", 1, 5)
        val app = GildedRose(arrayOf(item))
        app.updateQuality()

        app.assertQualityAndSellIn(
                expectedSellIn = 0,
                expectedQuality = 4
        )

        app.updateQuality()
        app.assertQualityAndSellIn(
                expectedSellIn = -1,
                expectedQuality = 2
        )
    }

    @Test
    internal fun `Quality decreases after sellin reached 0 and quality is 0`() {
        val item = Item("foo", 0, 2)
        val app = GildedRose(arrayOf(item))
        app.updateQuality()

        app.assertQualityAndSellIn(
                expectedSellIn = -1,
                expectedQuality = 0
        )
    }

    private fun GildedRose.assertQualityAndSellIn(expectedSellIn: Int, expectedQuality: Int, itemIndex: Int = 0) {
        val item = items[itemIndex]
        assertEquals(expectedSellIn, item.sellIn)
        assertEquals(expectedQuality, item.quality)
    }
}


