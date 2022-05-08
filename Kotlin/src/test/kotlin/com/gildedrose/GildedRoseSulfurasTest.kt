package com.gildedrose

import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.params.provider.Arguments

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
internal class GildedRoseSulfurasTest : GildedRoseBaseTest(){

    // Below are all testcases for this test, with the following arguments:
    // - name
    // - initialSellIn
    // - initialQuality
    // - numberDays
    // - resultingSellIn
    // - resultingQuality
    override val combinationsToTest = arrayOf(
        Arguments.of("Sulfuras, Hand of Ragnaros", 5, 5, 1, 5, 5),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", 5, 5, 2, 5, 5),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", 5, 5, 100, 5, 5),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", -1, 5, 1, -1, 5),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", -1, -1, 1, -1, -1),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", 100, -1, 1, 100, -1),// sellIn and quality always stays unchanged
        Arguments.of("Sulfuras, Hand of Ragnaros", 100, 100, 1, 100, 100),// sellIn and quality always stays unchanged
    )

}