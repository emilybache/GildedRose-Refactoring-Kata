package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("fixme", app.items[0].name);
    }

    @Test
    void testUpdateQualityNormalItem() {
        Item[] items = new Item[] { new Item("foo", 5, 10) };
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(9, items[0].quality);
        assertEquals(4, items[0].sellIn);
    }

    @Test
    void testUpdateQualityAgedBrie() {
        Item[] items = new Item[] { new Item("Aged Brie", 5, 10) };
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(11, items[0].quality);
        assertEquals(4, items[0].sellIn);
    }

    @Test
    void testUpdateQualitySulfuras() {
        Item[] items = new Item[] { new Item("Sulfuras, Hand of Ragnaros", 0, 80) };
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(80, items[0].quality);
        assertEquals(0, items[0].sellIn);
    }

}
