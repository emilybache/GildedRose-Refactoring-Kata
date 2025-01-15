package com.gildedrose;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class GildedRoseTest {
    @Test
    void test() {
        Item[] items = new Item[] {new Item("foo", 0, 0)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.getItems()[0].name);
    }
}
