package com.gildedrose

import org.assertj.core.api.Assertions.assertThat
import kotlin.random.Random

/*
TODO:
 - const voor maxNrDays, MaxNrItems, MaxInitialQuality, MaxInitialQuality
 - run als unit test, draai 1 minuut, of 1.000.000 keer
 - comment: deze kan flaky zijn, maar bij 1.000.000 is die kans wel erg klein

 */

val productLists = listOf(
    "Aged Brie",
    "Sulfuras, Hand of Ragnaros",
    "Backstage passes to a TAFKAL80ETC concert",
    "Foo"
)

val random = Random(System.currentTimeMillis())


fun main(args: Array<String>) {
    (0..Int.MAX_VALUE).forEach(){
        val nrItemsToTest = random.nextInt(1,5)
        val nrDays = random.nextInt(1,100)
        val initialItems = createRandomItemList(nrItemsToTest)
        val legacyItems = initialItems.deepClone()
        val refacturedItems = legacyItems.deepClone()

        val legacyApp = GildedRoseLegacy(legacyItems)
        val refactoredApp = GildedRose(refacturedItems)

        println("testing round $it, with $nrItemsToTest items and $nrDays days")
        (1..nrDays).forEach {
            legacyApp.updateQuality()
            refactoredApp.updateQuality()
        }

        val testDescription : String = describeTestcase(initialItems, nrDays, refactoredApp.items, legacyApp.items)
        val actialItemsAsString = describeItems(refactoredApp.items)
        val expectedItemsAsString = describeItems(legacyApp.items)
        assertThat(actialItemsAsString).`as`("The following testcase failed: \n$testDescription").isEqualTo(expectedItemsAsString)
    }
}

fun describeTestcase(initialItems: Array<Item>, nrDays: Int, actualItems: Array<Item>, expectedItems: Array<Item>) =
    "Initial items: \n${describeItems(initialItems)}\n\n"+
    "expected items after $nrDays days:\n"+
    "${describeItems(expectedItems)}\n\n"+
    "actual items after $nrDays days:\n"+
    describeItems(actualItems)

fun describeItems(items: Array<Item>) = items.map { " -${it.name}: sellIn:${it.sellIn} quality:${it.quality}" }.joinToString("\n")

private fun createRandomItem(): Item{
    val nameIndex = random.nextInt(productLists.size)
    val name = productLists[nameIndex]
    val sellIn =random.nextInt(-10, 100)
    val quality = random.nextInt(100)
    return Item(name,sellIn,quality)
}

private fun Item.clone() = Item(name,sellIn,quality)

private fun Array<Item>.deepClone() = map { it.clone() } .toTypedArray()


private fun createRandomItemList(count: Int) = (0..count).map { createRandomItem() }.toTypedArray()
