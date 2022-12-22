package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void agedBrieIncreasesInQualityByOneEachDay() {
        Item[] items = new Item[] { new Item(GildedRose.AGED_BRIE, 2, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, 1);
        app.updateQuality();
        assertEquals(app.items[0].quality, 2);
    }
}
