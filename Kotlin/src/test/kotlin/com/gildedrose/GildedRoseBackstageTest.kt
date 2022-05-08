package com.gildedrose

import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.util.stream.Stream

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
internal class GildedRoseBackstageTest : GildedRoseBaseTest(){


    private val combinationsToTest = arrayOf(
        // tests where sellIn is initially less then the quality
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 1, 11, 26),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 2, 10, 27),// 10 days before concert, quality goes up by 2 each day
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 3, 9, 29),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 4, 8, 31),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 5, 7, 33),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 6, 6, 35),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 7, 5, 37),// 5 days before concert, quality goes up by 3 each day
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 8, 4, 40),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 9, 3, 43),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 10, 2, 46),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 11, 1, 49),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 12, 0, 50),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 13, -1, 0), // quality drops to zero after the concert
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 14, -2, 0),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 25, 100, -88, 0),

        // tests where sellIn is initially larger then the quality
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 1, 11, 6),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 2, 10, 7),// 10 days before concert, quality goes up by 2 each day
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 3, 9, 9),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 4, 8, 11),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 5, 7, 13),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 6, 6, 15),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 7, 5, 17),// 5 days before concert, quality goes up by 3 each day
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 8, 4, 20),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 9, 3, 23),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 10, 2, 26),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 11, 1, 29),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 12, 0, 32),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 13, -1, 0), // quality drops to zero after the concert
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 14, -2, 0),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 5, 100, -88, 0),

        // tests other edge cases
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 12, 45, 12, 0, 50),// quality is never above 50
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", -1, 45, 1, -2, 0),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", -1, 60, 1, -2, 0),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 10, 60, 1, 9, 60), // quality stays at 60
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 82, 78, 82, 0, 78),
        Arguments.of("Backstage passes to a TAFKAL80ETC concert", 39, 17, 31, 8, 50),

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