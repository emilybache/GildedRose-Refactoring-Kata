package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ConjuredItemTest extends ItemTest{

    @Override
    @Test
    void sellInDecreaseTest() {
        Item[] items = new Item[] { new Item("Conjured", 2, 10) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertEquals(8, app.items[0].quality);
        assertEquals(1, app.items[0].sellIn);

        app.updateQuality();

        assertEquals(6, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);
    }

    @Override
    @Test
    void afterSellInQualityChangeTest() {
        Item[] items = new Item[] { new Item("Conjured", 1, 20) };
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(18, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);

        app.updateQuality();

        assertEquals(14, app.items[0].quality);
        assertEquals(-1, app.items[0].sellIn);
    }
}
