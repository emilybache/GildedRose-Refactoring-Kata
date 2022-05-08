package com.gildedrose

import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.params.provider.Arguments

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
internal class GildedRoseDefaultTest : GildedRoseBaseTest() {

    // Below are all testcases for this test, with the following arguments:
    // - name
    // - initialSellIn
    // - initialQuality
    // - numberDays
    // - resultingSellIn
    // - resultingQuality
    override val combinationsToTest: Array<Arguments> = arrayOf(
        // tests where sellIn and quality are initially the same
        Arguments.of("foo", 5, 5, 1, 4, 4),
        Arguments.of("foo", 5, 5, 2, 3, 3),
        Arguments.of("foo", 5, 5, 3, 2, 2),
        Arguments.of("foo", 5, 5, 4, 1, 1),
        Arguments.of("foo", 5, 5, 5, 0, 0),// after here, the quality stays at 0
        Arguments.of("foo", 5, 5, 6, -1, 0),
        Arguments.of("foo", 5, 5, 7, -2, 0),

        // tests where sellIn is initially less then the quality
        Arguments.of("foo", 5, 15, 1, 4, 14),
        Arguments.of("foo", 5, 15, 2, 3, 13),
        Arguments.of("foo", 5, 15, 3, 2, 12),
        Arguments.of("foo", 5, 15, 4, 1, 11),
        Arguments.of("foo", 5, 15, 5, 0, 10),// after here, the quality goes twice at fast
        Arguments.of("foo", 5, 15, 6, -1, 8),
        Arguments.of("foo", 5, 15, 7, -2, 6),
        Arguments.of("foo", 5, 15, 8, -3, 4),
        Arguments.of("foo", 5, 15, 9, -4, 2),
        Arguments.of("foo", 5, 15, 10, -5, 0),// after here, the quality stays at 0
        Arguments.of("foo", 5, 15, 11, -6, 0),
        Arguments.of("foo", 5, 15, 12, -7, 0),

        // tests where sellIn is initially larger then the quality
        Arguments.of("foo", 15, 5, 1, 14, 4),
        Arguments.of("foo", 15, 5, 2, 13, 3),
        Arguments.of("foo", 15, 5, 3, 12, 2),
        Arguments.of("foo", 15, 5, 4, 11, 1),
        Arguments.of("foo", 15, 5, 5, 10, 0),// after here, the quality stays at 0
        Arguments.of("foo", 15, 5, 6, 9, 0),
        Arguments.of("foo", 15, 5, 7, 8, 0),

        // tests other edge cases
        Arguments.of("foo", -1, 15, 1, -2, 13),
        Arguments.of("foo", -1, -1, 1, -2, -1),
        Arguments.of("foo", -1, -2, 1, -2, -2),
        Arguments.of("foo", -1, -2, 3, -4, -2),
        Arguments.of("foo", 100, -1, 1, 99, -1),
        Arguments.of("foo", 100, 100, 1, 99, 99),
    )

}