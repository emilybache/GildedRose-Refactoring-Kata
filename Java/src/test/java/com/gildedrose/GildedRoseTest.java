package com.gildedrose;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    @DisplayName("Test Aged Brie functionality")
    void testAgedBrie() {
        Item[] items = new Item[] { new Item("Aged Brie", 10, 80) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(Constants.AgedBrie, app.items[0].name);
        assertEquals(9, app.items[0].sellIn);
        assertEquals(80, app.items[0].quality);
    }

    @Test
    @DisplayName("Test BackStage functionality")
    void testBackStageSellIn() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 20, 50) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(Constants.Backstage, app.items[0].name);
        assertEquals(19, app.items[0].sellIn);
        assertEquals(50, app.items[0].quality);
    }

    @Test
    @DisplayName("Test BackStage Quality functionality")
    void testBackStageQuality() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 10, 45) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(Constants.Backstage, app.items[0].name);
        assertEquals(9, app.items[0].sellIn);
        assertEquals(47, app.items[0].quality);
    }

    @Test
    @DisplayName("Test BackStage Quality with least functionality")
    void testBackStageQualitywithLeast() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(Constants.Backstage, app.items[0].name);
        assertEquals(4, app.items[0].sellIn);
        assertEquals(50, app.items[0].quality);
    }

    @Test
    @DisplayName("Test Other functionality")
    void testOther() {
        Item[] items = new Item[] { new Item("Shan", 10, 20) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Shan", app.items[0].name);
        assertEquals(9, app.items[0].sellIn);
        assertEquals(19, app.items[0].quality);
    }

    @Test
    @DisplayName("Test Conjured functionality")
    void testConjured() {
        Item[] items = new Item[] { new Item("Conjured", 10, 10) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(Constants.Conjured, app.items[0].name);
        assertEquals(9, app.items[0].sellIn);
        assertEquals(8, app.items[0].quality);
    }
}
