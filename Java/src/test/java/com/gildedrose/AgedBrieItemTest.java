package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AgedBrieItemTest extends ItemTest {

    @Override
    @Test
    void sellInDecreaseTest() {
        Item[] items = new Item[] { new Item("Aged Brie", 2, 2) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertEquals(3, app.items[0].quality);
        assertEquals(1, app.items[0].sellIn);

        app.updateQuality();

        assertEquals(4, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);
    }

    @Override
    @Test
    void afterSellInQualityChangeTest() {
        Item[] items = new Item[] { new Item("Aged Brie", 1, 2) };
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(3, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);

        app.updateQuality();

        assertEquals(5, app.items[0].quality);
        assertEquals(-1, app.items[0].sellIn);
    }

    @Test
    void qualityNotAbove50Test() {
        Item[] items = new Item[] { new Item("Aged Brie", 2, 49) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertEquals(50, app.items[0].quality);
        assertEquals(1, app.items[0].sellIn);

        app.updateQuality();

        assertEquals(50, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);
    }
}
