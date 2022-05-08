package com.gildedrose

import org.assertj.core.api.Assertions
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.util.stream.Stream

abstract class GildedRoseBaseTest {

    abstract val combinationsToTest : Array<Arguments> // this array is created in all subclass tests

    @ParameterizedTest(name = "{0}: initial sellIn:{1} and initial quality:{2}, after {3} days: sellIn:{4} and quality {5}, using Legacy algorithm")
    @MethodSource("combinationsSource")
    fun `given input item, when some days are passed, then the item state is correctly modified, using Legacy algorithm`(
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

        (1..numberDays).forEach { _ ->
            app.updateQuality()
        }
        Assertions.assertThat(item.name).isEqualTo(name)
        Assertions.assertThat(item.sellIn).isEqualTo(resultingSellIn)
        Assertions.assertThat(item.quality).isEqualTo(resultingQuality)
    }

    @ParameterizedTest(name = "{0}: initial sellIn:{1} and initial quality:{2}, after {3} days: sellIn:{4} and quality {5}, using Refactored algorithm")
    @MethodSource("combinationsSource")
    fun `given input item, when some days are passed, then the item state is correctly modified, using Refactored algorithm`(
        name: String,
        initialSellIn: Int,
        initialQuality: Int,
        numberDays: Int,
        resultingSellIn: Int,
        resultingQuality: Int
    ) {
        val item = Item(name, initialSellIn, initialQuality)
        val items = arrayOf(item)
        val app = GildedRoseLegacy(items)

        (1..numberDays).forEach { _ ->
            app.updateQuality()
        }
        Assertions.assertThat(item.name).isEqualTo(name)
        Assertions.assertThat(item.sellIn).isEqualTo(resultingSellIn)
        Assertions.assertThat(item.quality).isEqualTo(resultingQuality)
    }

    private fun combinationsSource(): Stream<Arguments> = Stream.of(*combinationsToTest)


}