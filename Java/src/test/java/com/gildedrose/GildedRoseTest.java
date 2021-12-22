package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void testAgedBrieItems() {
        Item[] items = new Item[] {
            new Item("Aged Brie", 100, 10),
            new Item("Aged Brie", 10, 40),
            new Item("Aged Brie", 2, 49),
            new Item("Aged Brie", 1, 50),
        };

        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertItem(items[0], "Aged Brie", 99, 11);
        assertItem(items[1], "Aged Brie", 9, 41);
        assertItem(items[2], "Aged Brie", 1, 50);
        assertItem(items[3], "Aged Brie", 0, 50);
    }

    @Test
    void testSulfurasItems() {
        Item[] items = new Item[] {
            new Item("Sulfuras, Hand of Ragnaros", 100, 80),
            new Item("Sulfuras, Hand of Ragnaros", -1, 80),
        };

        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertItem(items[0], "Sulfuras, Hand of Ragnaros", 100, 80);
        assertItem(items[1], "Sulfuras, Hand of Ragnaros", -1, 80);
    }

    @Test
    void testBackstagePassesItems() {
        Item[] items = new Item[] {
            new Item("Backstage passes to a TAFKAL80ETC concert", 11, 5),
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 5),
            new Item("Backstage passes to a TAFKAL80ETC concert", 9, 5),
            new Item("Backstage passes to a TAFKAL80ETC concert", 6, 5),
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 5),
            new Item("Backstage passes to a TAFKAL80ETC concert", 4, 5),
            new Item("Backstage passes to a TAFKAL80ETC concert", 1, 5),
            new Item("Backstage passes to a TAFKAL80ETC concert", 0, 5),
        };

        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertItem(items[0], "Backstage passes to a TAFKAL80ETC concert", 10, 6);
        assertItem(items[1], "Backstage passes to a TAFKAL80ETC concert", 9, 7);
        assertItem(items[2], "Backstage passes to a TAFKAL80ETC concert", 8, 7);
        assertItem(items[3], "Backstage passes to a TAFKAL80ETC concert", 5, 7);
        assertItem(items[4], "Backstage passes to a TAFKAL80ETC concert", 4, 8);
        assertItem(items[5], "Backstage passes to a TAFKAL80ETC concert", 3, 8);
        assertItem(items[6], "Backstage passes to a TAFKAL80ETC concert", 0, 8);
        assertItem(items[7], "Backstage passes to a TAFKAL80ETC concert", -1, 0);
    }

    @Test
    void testStandardItems() {
        Item[] items = new Item[] {
            new Item("NoParticularName", 25, 30),
            new Item("NoParticularName", 1, 30),
            new Item("NoParticularName", 0, 30),
            new Item("NoParticularName", -1, 30),
            new Item("NoParticularName", 1, 1),
            new Item("NoParticularName", 0, 0),
        };

        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertItem(items[0], "NoParticularName", 24, 29);
        assertItem(items[1], "NoParticularName", 0, 29);
        assertItem(items[2], "NoParticularName", -1, 28);
        assertItem(items[3], "NoParticularName", -2, 28);
        assertItem(items[4], "NoParticularName", 0, 0);
        assertItem(items[5], "NoParticularName", -1, 0);
    }

    @Test
    void testConjuredItems() {
        Item[] items = new Item[] {
            new Item("Conjured Mana Cake", 25, 30),
            new Item("Conjured Mana Cake", 1, 30),
            new Item("Conjured Mana Cake", 0, 30),
            new Item("Conjured Mana Cake", -1, 30),
            new Item("Conjured Mana Cake", 1, 1),
            new Item("Conjured Mana Cake", 0, 0),
        };

        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertItem(items[0], "Conjured Mana Cake", 24, 28);
        assertItem(items[1], "Conjured Mana Cake", 0, 28);
        assertItem(items[2], "Conjured Mana Cake", -1, 26);
        assertItem(items[3], "Conjured Mana Cake", -2, 26);
        assertItem(items[4], "Conjured Mana Cake", 0, 0);
        assertItem(items[5], "Conjured Mana Cake", -1, 0);
    }

    private void assertItem(Item item, String name, int sellIn, int quality) {
        assertAll(
            () -> assertEquals(name, item.name, "item name"),
            () -> assertEquals(sellIn, item.sellIn, "item sellIn"),
            () -> assertEquals(quality, item.quality, "item quality")
        );
    }
}
