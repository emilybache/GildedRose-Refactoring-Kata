package com.gildedrose.acceptance

import com.gildedrose.GildedRose
import com.gildedrose.Item
import com.gildedrose.core.TestToFileHandler
import com.gildedrose.core.TestUtils
import org.junit.Assert.assertEquals
import org.junit.Test

class ThirtyDaysAcceptanceTests {

    private val testToFileHandler = TestToFileHandler("ThirtyDays")

    @Test
    fun withInitialTextFixture_whenRunForThirtyDays_itShouldHaveSameOutputAsLegacyCode() {
        val initialTextFixture = TestUtils.fixture

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