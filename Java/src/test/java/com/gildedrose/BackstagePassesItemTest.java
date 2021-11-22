package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BackstagePassesItemTest extends ItemTest{

    @Override
    @Test
    void sellInDecreaseTest() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 11, 0),
            new Item("Backstage passes to a TAFKAL80ETC concert", 6, 10) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertEquals(1, app.items[0].quality);
        assertEquals(10, app.items[0].sellIn);
        assertEquals(12, app.items[1].quality);
        assertEquals(5, app.items[1].sellIn);

        app.updateQuality();

        assertEquals(3, app.items[0].quality);
        assertEquals(9, app.items[0].sellIn);
        assertEquals(15, app.items[1].quality);
        assertEquals(4, app.items[1].sellIn);
    }

    @Override
    @Test
    void afterSellInQualityChangeTest() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 1, 20) };
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(23, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);

        app.updateQuality();

        assertEquals(0, app.items[0].quality);
        assertEquals(-1, app.items[0].sellIn);
    }

    @Test
    void qualityNotAbove50Test() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 2, 49) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertEquals(50, app.items[0].quality);
        assertEquals(1, app.items[0].sellIn);

        app.updateQuality();

        assertEquals(50, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);
    }
}
