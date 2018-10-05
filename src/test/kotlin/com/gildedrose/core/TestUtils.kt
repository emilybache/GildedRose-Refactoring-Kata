package com.gildedrose.core

import com.gildedrose.Item
import java.util.*

object TestUtils {

    val fixture
        get() = arrayOf(
                Item("+5 Dexterity Vest", 10, 20),
                Item("Aged Brie", 2, 0),
                Item("Elixir of the Mongoose", 5, 7),
                Item("Sulfuras, Hand of Ragnaros", 0, 80),
                Item("Sulfuras, Hand of Ragnaros", -1, 80),
                Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
                Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
                Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
                Item("Conjured Mana Cake", 3, 6))

    fun generateRandomName() = UUID.randomUUID().toString()

    fun generateRandomNumber() = (0 until 9999).shuffled().last()

    fun pickRandomItem(items: List<Item>) = items[items.indices.shuffled().last()]
}