package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void testNormalDegrade() {
        Item testItem = new Item("test_item", 3, 10);
        Item[] items = new Item[] {testItem};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item updatedItem = app.items[0];
        assertEquals(2, updatedItem.sellIn);
        assertEquals(9, updatedItem.quality);
    }

    @Test
    void testPastSellInDegrade() {
        Item testItem = new Item("test_item", 0, 10);
        Item[] items = new Item[] {testItem};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item updatedItem = app.items[0];
        assertEquals(-1, updatedItem.sellIn);
        assertEquals(8, updatedItem.quality);
    }

    @Test
    void testQualityMustNotEverGoUnderZero() {
        Item testItem = new Item("test_item", 5, 0);
        Item[] items = new Item[] {testItem};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item updatedItem = app.items[0];
        assertEquals(0, testItem.quality);
    }

    @Test
    void testAgedBrieGetsBetterWithAge() {
        Item testItem = new Item("Aged Brie", 5, 10);
        Item[] items = new Item[] {testItem};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item updatedItem = app.items[0];
        assertEquals(4, updatedItem.sellIn);
        assertEquals(11, updatedItem.quality);
    }

    @Test
    void testQualityIsCappedAt50() {
        Item testItem = new Item("Aged Brie", 5, 50);
        Item[] items = new Item[] {testItem};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item updatedItem = app.items[0];
        assertEquals(50, updatedItem.quality);
    }

    @Test
    void testBackstagePass() {
        Item testItem = new Item("Backstage passes to a TAFKAL80ETC concert", 6, 10);
        Item[] items = new Item[] {testItem};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item updatedItem = app.items[0];
        assertEquals(12, updatedItem.quality);
    }

    @Test
    void testBackstagePassWhenLessThan3DaysRemaining() {
        Item testItem = new Item("Backstage passes to a TAFKAL80ETC concert", 3, 10);
        Item[] items = new Item[] {testItem};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item updatedItem = app.items[0];
        assertEquals(13, updatedItem.quality);
    }

    @Test
    void testExpiredBackstagePass() {
        Item testItem = new Item("Backstage passes to a TAFKAL80ETC concert", 0, 10);
        Item[] items = new Item[] {testItem};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item updatedItem = app.items[0];
        assertEquals(0, updatedItem.quality);
    }
}
