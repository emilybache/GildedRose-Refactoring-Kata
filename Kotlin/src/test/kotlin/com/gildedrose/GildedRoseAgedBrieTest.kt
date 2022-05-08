package com.gildedrose

import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.params.provider.Arguments

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
internal class GildedRoseAgedBrieTest : GildedRoseBaseTest() {

    // Below are all testcases for this test, with the following arguments:
    // - name
    // - initialSellIn
    // - initialQuality
    // - numberDays
    // - resultingSellIn
    // - resultingQuality
    override val combinationsToTest = arrayOf(
        // tests where sellIn and quality are initially the same
        Arguments.of("Aged Brie", 5, 5, 1, 4, 6),
        Arguments.of("Aged Brie", 5, 5, 2, 3, 7),
        Arguments.of("Aged Brie", 5, 5, 3, 2, 8),
        Arguments.of("Aged Brie", 5, 5, 4, 1, 9),
        Arguments.of("Aged Brie", 5, 5, 5, 0, 10),// after here, the quality goes twice as fast
        Arguments.of("Aged Brie", 5, 5, 6, -1, 12),
        Arguments.of("Aged Brie", 5, 5, 7, -2, 14),
        Arguments.of("Aged Brie", 5, 5, 8, -3, 16),
        // skip a few days
        Arguments.of("Aged Brie", 5, 5, 24, -19, 48),
        Arguments.of("Aged Brie", 5, 5, 25, -20, 50),// after here, the quality stays as 50
        Arguments.of("Aged Brie", 5, 5, 26, -21, 50),
        Arguments.of("Aged Brie", 5, 5, 27, -22, 50),

        // tests where sellIn is initially less then the quality
        Arguments.of("Aged Brie", 5, 15, 1, 4, 16),
        Arguments.of("Aged Brie", 5, 15, 2, 3, 17),
        Arguments.of("Aged Brie", 5, 15, 3, 2, 18),
        Arguments.of("Aged Brie", 5, 15, 4, 1, 19),
        Arguments.of("Aged Brie", 5, 15, 5, 0, 20),// after here, the quality goes twice as fast
        Arguments.of("Aged Brie", 5, 15, 6, -1, 22),
        Arguments.of("Aged Brie", 5, 15, 7, -2, 24),
        Arguments.of("Aged Brie", 5, 15, 8, -3, 26),
        // skip a few days
        Arguments.of("Aged Brie", 5, 15, 19, -14, 48),
        Arguments.of("Aged Brie", 5, 15, 20, -15, 50),// after here, the quality stays as 50
        Arguments.of("Aged Brie", 5, 15, 21, -16, 50),
        Arguments.of("Aged Brie", 5, 15, 22, -17, 50),

        // tests where sellIn is initially larger then the quality
        Arguments.of("Aged Brie", 15, 5, 1, 14, 6),
        Arguments.of("Aged Brie", 15, 5, 2, 13, 7),
        Arguments.of("Aged Brie", 15, 5, 3, 12, 8),
        Arguments.of("Aged Brie", 15, 5, 4, 11, 9),
        Arguments.of("Aged Brie", 15, 5, 5, 10, 10),
        Arguments.of("Aged Brie", 15, 5, 6, 9, 11),
        Arguments.of("Aged Brie", 15, 5, 7, 8, 12),
        Arguments.of("Aged Brie", 15, 5, 8, 7, 13),
        // skip a few days
        Arguments.of("Aged Brie", 15, 5, 13, 2, 18),
        Arguments.of("Aged Brie", 15, 5, 14, 1, 19),
        Arguments.of("Aged Brie", 15, 5, 15, 0, 20),// after here, the quality goes twice as fast
        Arguments.of("Aged Brie", 15, 5, 16, -1, 22),
        Arguments.of("Aged Brie", 15, 5, 17, -2, 24),
        Arguments.of("Aged Brie", 15, 5, 29, -14, 48),
        Arguments.of("Aged Brie", 15, 5, 30, -15, 50),// after here, the quality stays as 50
        Arguments.of("Aged Brie", 15, 5, 31, -16, 50),
        Arguments.of("Aged Brie", 15, 5, 32, -17, 50),
        Arguments.of("Aged Brie", -1, 49, 1, -2, 50),

        // tests other edge cases
        Arguments.of("Aged Brie", -1, 15, 1, -2, 17),
        Arguments.of("Aged Brie", -1, -10, 1, -2, -8),
        Arguments.of("Aged Brie", -1, -1, 1, -2, 1),
        Arguments.of("Aged Brie", -1, -2, 1, -2, 0),
        Arguments.of("Aged Brie", -1, -2, 3, -4, 4),
        Arguments.of("Aged Brie", 100, -1, 1, 99, 0),
        Arguments.of("Aged Brie", 100, 100, 1, 99, 100),
        Arguments.of("Aged Brie", 1, 0, 16, -15, 31),
        )
}