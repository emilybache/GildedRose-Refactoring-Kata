package com.gildedrose

import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test
import kotlin.random.Random

private const val NR_TESTS_TO_RUN = 100_000 // 100.000 random tests should be enough to cover any untested cases by the normal unit tests
private const val MAX_NR_ITEMS = 5
private const val MAX_NR_DAYS = 100
private const val MAX_INITIAL_QUALITY = 100
private const val MIN_INITIAL_SELLIN = -10
private const val MAX_INITIAL_SELLIN = 100
private val PRODUCTS_LIST = listOf(
    "Aged Brie",
    "Sulfuras, Hand of Ragnaros",
    "Backstage passes to a TAFKAL80ETC concert",
    "Foo"
)
private val random = Random(System.currentTimeMillis())

class RandomizedRegressionTest {
    @Test
    fun `test random combinations and compare the result from the legacy code with the refactored code`() {
        (0..NR_TESTS_TO_RUN).forEach{
            // create new random combination of items
            val nrItemsToTest = random.nextInt(1,MAX_NR_ITEMS)
            val nrDays = random.nextInt(1,MAX_NR_DAYS)
            val initialItems = createRandomItemList(nrItemsToTest)
            val legacyItems = initialItems.deepClone()
            val refacturedItems = legacyItems.deepClone()

            // clone the items for the legacy and the refactored implementation
            val legacyApp = GildedRoseLegacy(legacyItems)
            val refactoredApp = GildedRose(refacturedItems)

            // update the quality for a number of days for both the legacy and the refactored implementation
            println("testing round $it, with $nrItemsToTest items and $nrDays days")
            (1..nrDays).forEach { _ ->
                legacyApp.updateQuality()
                refactoredApp.updateQuality()
            }

            // compare the result of the legacy and the refactored implementation
            val testDescription : String = describeTestcase(initialItems, nrDays, refactoredApp.items, legacyApp.items)
            val actialItemsAsString = describeItems(refactoredApp.items)
            val expectedItemsAsString = describeItems(legacyApp.items)
            assertThat(actialItemsAsString).`as`("The following testcase failed: \n$testDescription").isEqualTo(expectedItemsAsString)
        }
    }

    private fun describeTestcase(initialItems: Array<Item>, nrDays: Int, actualItems: Array<Item>, expectedItems: Array<Item>) =
        "Initial items: \n${describeItems(initialItems)}\n\n"+
                "expected items after $nrDays days:\n"+
                "${describeItems(expectedItems)}\n\n"+
                "actual items after $nrDays days:\n"+
                describeItems(actualItems)

    private fun describeItems(items: Array<Item>) = items.joinToString("\n") { " -${it.name}: sellIn:${it.sellIn} quality:${it.quality}" }

    private fun createRandomItem(): Item{
        val nameIndex = random.nextInt(PRODUCTS_LIST.size)
        val name = PRODUCTS_LIST[nameIndex]
        val sellIn =random.nextInt(MIN_INITIAL_SELLIN, MAX_INITIAL_SELLIN)
        val quality = random.nextInt(MAX_INITIAL_QUALITY)
        return Item(name,sellIn,quality)
    }

    private fun Item.clone() = Item(name,sellIn,quality)

    private fun Array<Item>.deepClone() = map { it.clone() } .toTypedArray()

    private fun createRandomItemList(count: Int) = (0..count).map { createRandomItem() }.toTypedArray()


}
