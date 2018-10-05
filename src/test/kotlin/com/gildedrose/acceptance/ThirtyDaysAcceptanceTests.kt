package com.gildedrose.acceptance

import com.gildedrose.GildedRose
import com.gildedrose.Item
import com.gildedrose.core.TestToFileHandler
import org.junit.Assert.assertEquals
import org.junit.Test

class ThirtyDaysAcceptanceTests {

    private val testToFileHandler = TestToFileHandler("ThirtyDays")

    @Test
    fun withInitialTextFixture_whenRunForThirtyDays_itShouldHaveSameOutputAsLegacyCode() {
        val initialTextFixture = arrayOf(
                Item("+5 Dexterity Vest", 10, 20), //
                Item("Aged Brie", 2, 0), //
                Item("Elixir of the Mongoose", 5, 7), //
                Item("Sulfuras, Hand of Ragnaros", 0, 80), //
                Item("Sulfuras, Hand of Ragnaros", -1, 80),
                Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
                Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
                Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
                // this conjured item does not work properly yet
                Item("Conjured Mana Cake", 3, 6))

        val output = runWithItemsForDays(initialTextFixture, 30)

        assertEquals(output, testToFileHandler.read())
    }

    private fun runWithItemsForDays(items: Array<Item>, days: Int): String {
        var output = ""

        val app = GildedRose(items)

        for (i in 0 until days) {
            output += "\nDAY $i\n"

            items.forEach {
                val newLine = if (it == items.last()) "" else "\n"
                output += "$it$newLine"
            }

            app.updateQuality()
        }

        return output
    }
}