package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[] { new Item("fixme", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("fixme", app.items[0].name);
    }

    @Test
    void agedBrieTest() {
        Item[] items = new Item[] { new Item("Aged Brie", 2, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Aged Brie", app.items[0].name);
        assertEquals(1, app.items[0].quality);
        assertEquals(1, app.items[0].sellIn);
        app.updateQuality();
        assertEquals(2, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);
    }
    @Test
    void conjuredTest() {
        Item[] items = new Item[] { new Item("Conjured Mana Cake", 3, 6) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Conjured Mana Cake", app.items[0].name);
        assertEquals(4, app.items[0].quality);
        assertEquals(2, app.items[0].sellIn);
        app.updateQuality();
        assertEquals(2, app.items[0].quality);
        assertEquals(1, app.items[0].sellIn);
    }
}
