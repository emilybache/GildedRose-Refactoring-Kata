package com.gildedrose

import org.assertj.core.api.Assertions

open class GildedRoseBaseTest {
    fun testGildedRose(
        name: String,
        initialSellIn: Int,
        initialQuality: Int,
        numberDays: Int,
        resultingSellIn: Int,
        resultingQuality: Int
    ) {
        val item = Item(name, initialSellIn, initialQuality)
        val items = arrayOf(item)
        val app = GildedRose(items)

        (1..numberDays).forEach() {
            app.updateQuality()
        }
        Assertions.assertThat(item.name).isEqualTo(name)
        Assertions.assertThat(item.sellIn).isEqualTo(resultingSellIn)
        Assertions.assertThat(item.quality).isEqualTo(resultingQuality)
    }

}