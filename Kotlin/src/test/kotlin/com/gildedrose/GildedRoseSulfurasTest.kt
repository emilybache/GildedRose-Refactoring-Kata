package com.gildedrose

import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.util.stream.Stream

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
internal class GildedRoseSulfurasTest : GildedRoseBaseTest(){

    private val combinationsToTest = arrayOf(
        Arguments.of("Sulfuras, Hand of Ragnaros", 5, 5, 1, 5, 5),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", 5, 5, 2, 5, 5),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", 5, 5, 100, 5, 5),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", -1, 5, 1, -1, 5),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", -1, -1, 1, -1, -1),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", 100, -1, 1, 100, -1),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", 100, 100, 1, 100, 100),// sellIn and quality always stays unchanged
    )

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
        testGildedRose(name, initialSellIn, initialQuality, numberDays, resultingSellIn, resultingQuality)
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
        testGildedRose(name, initialSellIn, initialQuality, numberDays, resultingSellIn, resultingQuality)
    }
    
    fun combinationsSource(): Stream<Arguments>  = Stream.of(*combinationsToTest)
}