package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    public void checkAgedBrieName() {
        Item[] items = new Item[] { new AgedBrie("AgedBrie", 0, 0) };
        GildedRose gildedRose = new GildedRose(items);
        gildedRose.updateQuality();
        assertEquals("AgedBrie", gildedRose.items[0].name);
    }

    @Test
    public void checkWrongItemName() {
        Item[] items = new Item[] { new Item("WrongItem", 0, 0) };
        GildedRose gildedRose = new GildedRose(items);
        gildedRose.updateQuality();
        assertEquals("WrongItem", gildedRose.items[0].name);
    }

}
