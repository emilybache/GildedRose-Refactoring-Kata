package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[]{new Item("foo", 0, 0)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("fixme", app.items[0].name);
    }

    //    public Item(String name, int sellIn, int quality) {
    @Test
    void conjuredItemQualityDecreaseTwice() {
        Item[] items = new Item[]{new Item("Conjured Cake", 0, 2), new Item("Aged Brie", 1, 30)};
        GildedRose glidedRose = new GildedRose(items);
        glidedRose.updateQuality();
        assertEquals(0, glidedRose.items[0].quality);
    }
    @Test
    void conjuredItemQualityShouldotBeNegative() {
        Item[] items = new Item[]{new Item("Conjured Cake", 0, 1), new Item("Aged Brie", 1, 30)};
        GildedRose glidedRose = new GildedRose(items);
        glidedRose.updateQuality();
        assertEquals(0, glidedRose.items[0].quality);
    }
}
