package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ItemTest {

    @Test
    void sellInDecreaseTest() {
        Item[] items = new Item[] { new Item("foo", 2, 2) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertEquals(1, app.items[0].quality);
        assertEquals(1, app.items[0].sellIn);

        app.updateQuality();

        assertEquals(0, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);
    }

    @Test
    void notNegativeQualityTest() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(0, app.items[0].quality);
        assertEquals(-1, app.items[0].sellIn);
    }

    @Test
    void afterSellInQualityChangeTest() {
        Item[] items = new Item[] { new Item("foo", 1, 20) };
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(19, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);

        app.updateQuality();

        assertEquals(17, app.items[0].quality);
        assertEquals(-1, app.items[0].sellIn);
    }
}
