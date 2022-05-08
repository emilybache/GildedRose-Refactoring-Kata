package com.gildedrose

import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.params.provider.Arguments

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
internal class GildedRoseBackstageTest : GildedRoseBaseTest(){

    // Below are all testcases for this test, with the following arguments:
    // - name
    // - initialSellIn
    // - initialQuality
    // - numberDays
    // - resultingSellIn
    // - resultingQuality
    override val combinationsToTest = arrayOf(
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

}