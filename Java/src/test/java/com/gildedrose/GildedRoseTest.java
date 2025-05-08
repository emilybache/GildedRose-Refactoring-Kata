package com.gildedrose;

import org.junit.jupiter.api.Test;

import static com.gildedrose.GildedRoseConstants.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class GildedRoseTest {
    @Test
    void agedBrieIncreasesInQuality() {
        Item[] items = new Item[] { new Item(AGED_BRIE, 2, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(1, items[0].quality);
        assertEquals(1, items[0].sellIn);
    }

    @Test
    void sulfurasDoesNotChange() {
        Item[] items = new Item[] { new Item(SULFURAS, 0, 80) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(80, items[0].quality);
        assertEquals(0, items[0].sellIn);
    }

    @Test
    void backstagePassIncreasesCorrectly() {
        Item[] items = new Item[] { new Item(BACKSTAGE_PASS, 5, 10) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(13, items[0].quality);
        assertEquals(4, items[0].sellIn);
    }
}
