package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void givenMultipleItems_whenDegradeItems_thenAllItemsDegradeCorrectly() {
        Item[] items = {
            new NormalItem("Normal Item", 10, 20),
            new AgedBrieItem("Aged Brie", 5, 10),
            new BackstagePassItem("Backstage Pass", 15, 30),
            new SulfurasItem("Sulfuras, Hand of Ragnaros", 0, 80)
        };
        var app = new GildedRose(items);
        app.degradeItems();

        assertEquals(9, items[0].sellIn);
        assertEquals(19, items[0].quality);
        assertEquals(4, items[1].sellIn);
        assertEquals(11, items[1].quality);
        assertEquals(14, items[2].sellIn);
        assertEquals(31, items[2].quality);
        assertEquals(0, items[3].sellIn);
        assertEquals(80, items[3].quality);
    }
}
