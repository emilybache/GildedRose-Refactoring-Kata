package com.gildedrose;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class GildedRoseTest {
    @Test
    void test() {
        Item[] items = new Item[] {new Item(GildedRose.CONJURED, -1, 4)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(GildedRose.CONJURED, app.getItems()[0].name);
        assertEquals(0, app.getItems()[0].quality);
        assertEquals(-2, app.getItems()[0].sellIn);
    }
}
