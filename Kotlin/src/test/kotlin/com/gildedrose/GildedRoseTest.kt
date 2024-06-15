package com.gildedrose

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertAll
import org.junit.jupiter.api.extension.ExtensionContext
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.Arguments.of
import org.junit.jupiter.params.provider.ArgumentsProvider
import org.junit.jupiter.params.provider.ArgumentsSource
import java.util.stream.Stream


internal class GildedRoseTest {

    @Test
    fun foo() {
        val items = listOf(Item("foo", 0, 0))
        val app = GildedRose(items)
        app.updateQuality()
        assertEquals("foo", app.items[0].name)
    }

    @Test
    fun `should execute without exception for empty list`() {
        val items = listOf<Item>()
        val app = GildedRose(items)
        app.updateQuality()
        assertAll(
            { assertEquals(items, app.items) }
        )
    }

    @ParameterizedTest
    @ArgumentsSource(GildedRoseArgumentsProvider::class)
    fun `should update quality`(item: Item, expectedQuality: Int, expectedSellIn: Int) {
        val itemName = item.name
        val items = listOf(item)
        val app = GildedRose(items)
        app.updateQuality()
        assertAll(
            { assertEquals(itemName, app.items[0].name) },
            { assertEquals(expectedQuality, app.items[0].quality) },
            { assertEquals(expectedSellIn, app.items[0].sellIn) }
        )
    }

    class GildedRoseArgumentsProvider : ArgumentsProvider {
        override fun provideArguments(context: ExtensionContext): Stream<out Arguments?> {
            return Stream.of(
                of(Item("+5 Dexterity Vest", 10, 20), 19, 9),
                of(Item("Aged Brie", 2, 0), 1, 1),
                of(Item("Elixir of the Mongoose", 5, 7), 6, 4),
                of(Item("Sulfuras, Hand of Ragnaros", 0, 80), 80, 0),
                of(Item("Sulfuras, Hand of Ragnaros", -1, 80), 80, -1),
                of(Item("Backstage passes to a TAFKAL80ETC concert", 15, 20), 21, 14),
                of(Item("Backstage passes to a TAFKAL80ETC concert", 10, 49), 50, 9),
                of(Item("Backstage passes to a TAFKAL80ETC concert", 5, 49), 50, 4),
                // this conjured item does not work properly yet
                // of(Item("Conjured Mana Cake", 3, 6), 5, 2), // TODO: enabled and fix after refactor
            )
        }
    }
}


