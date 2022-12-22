package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {
    private static final int SULFURAS_QUALITY = 80;
    @Test
    public void agedBrieIncreasesInQualityByOneEachDay() {
        Item[] items = new Item[] { new Item(GildedRose.AGED_BRIE, 2, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, 1);
        app.updateQuality();
        assertEquals(app.items[0].quality, 2);
    }

    @Test
    public void sulfurasSellInDateNeverChanges() {
        int sellIn = 10;
        Item[] items = new Item[] { new Item(GildedRose.SULFURAS, sellIn, SULFURAS_QUALITY) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].sellIn, sellIn);
        app.updateQuality();
        assertEquals(app.items[0].sellIn, sellIn);
    }

    @Test
    public void sulfurasValueNeverChanges() {
        Item[] items = new Item[] { new Item(GildedRose.SULFURAS, 10, SULFURAS_QUALITY) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, SULFURAS_QUALITY);
        app.updateQuality();
        assertEquals(app.items[0].quality, SULFURAS_QUALITY);
    }

    @Test
    public void sulfurasValueIsAlways80() {
        Item[] items = new Item[] { new Item(GildedRose.SULFURAS, 10, SULFURAS_QUALITY) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, SULFURAS_QUALITY);
    }
}
