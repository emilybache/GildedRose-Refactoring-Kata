package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[]{new Item("foo", 0, 0)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }

    @Test
    void normalItem() {
        GildedRose gildedRose = newGildedRose("normal item", 8, 30);

        for (int i = 0; i < 10; i++) {
            gildedRose.updateQuality();
        }

        assertEquals(-2, gildedRose.items[0].sellIn);
        assertEquals(18, gildedRose.items[0].quality);
    }

    @Test
    void agedBrieItem() {
        GildedRose gildedRose = newGildedRose("Aged Brie", 8, 30);

        for (int i = 0; i < 10; i++) {
            gildedRose.updateQuality();
        }

        assertEquals(-2, gildedRose.items[0].sellIn);
        assertEquals(42, gildedRose.items[0].quality);
    }

    @Test
    void backstageItem() {
        GildedRose gildedRose = newGildedRose("Backstage passes to a TAFKAL80ETC concert", 15, 10);

        for (int i = 0; i < 15; i++) {
            gildedRose.updateQuality();
        }

        assertEquals(0, gildedRose.items[0].sellIn);
        assertEquals(40, gildedRose.items[0].quality);
    }

    @Test
    void backstageItemExpired() {
        GildedRose gildedRose = newGildedRose("Backstage passes to a TAFKAL80ETC concert", 15, 10);

        for (int i = 0; i < 16; i++) {
            gildedRose.updateQuality();
        }

        assertEquals(-1, gildedRose.items[0].sellIn);
        assertEquals(0, gildedRose.items[0].quality);
    }

    @Test
    void conjuredItem() {
        GildedRose gildedRose = newGildedRose("Conjured", 8, 30);

        for (int i = 0; i < 10; i++) {
            gildedRose.updateQuality();
        }

        assertEquals(-2, gildedRose.items[0].sellIn);
        assertEquals(6, gildedRose.items[0].quality);
    }

    @Test
    void sulfurasItem() {
        GildedRose gildedRose = newGildedRose("Sulfuras, Hand of Ragnaros", 8, 80);

        for (int i = 0; i < 10; i++) {
            gildedRose.updateQuality();
        }

        assertEquals(8, gildedRose.items[0].sellIn);
        assertEquals(80, gildedRose.items[0].quality);
    }

    private GildedRose newGildedRose(String itemName, int itemSellIn, int itemQuality) {
        Item[] items = new Item[]{new Item(itemName, itemSellIn, itemQuality)};
        return new GildedRose(items);
    }
}
