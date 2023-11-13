package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }

    @Test
    void standardItems(){
        Item[] items = new Item[]{
            new Item("item1", 40, 40)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, 39);
        assertEquals(app.items[0].sellIn, 39);
    }

    @Test
    void qualityDegradationForExpired(){
        Item[] items = new Item[]{new Item("item1", 0, 40)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, 38);
    }

    @Test
    void neverNegative(){
        Item[] items = new Item[]{
            new Item("item1", 0, 0),
            new Item("item2", 10, 0),
            new Item("item2", 0, 1),
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, 0);
        assertEquals(app.items[1].quality, 0);
        assertEquals(app.items[2].quality, 0);
    }

    @Test
    void agedBrieIncreasedValue(){
        Item[] items = new Item[]{
            new Item("Aged Brie", 9, 0),
            new Item("Aged Brie", 0, 0)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, 1);
        assertEquals(app.items[1].quality, 2);
    }

    @Test
    void neverAbove50(){
        Item[] items = new Item[]{
            new Item("Aged Brie", 9, 50),
            new Item("Aged Brie", 0, 50),
            new Item("Aged Brie", 0, 49)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, 50);
        assertEquals(app.items[1].quality, 50);
        assertEquals(app.items[2].quality, 50);
    }

    @Test
    void sulfurasNeverDecreases(){
        //TODO: Sulfuras should be protected against instantiation with quality != 80
        Item[] items = new Item[]{
            new Item("Sulfuras, Hand of Ragnaros", 100, 80),
            new Item("SULFURAS, pants of Ragnar Lothbrok", 200, 80)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, 80);
        assertEquals(app.items[0].sellIn, 100);
        assertEquals(app.items[1].quality, 80);
        assertEquals(app.items[1].sellIn, 200);
    }

    @Test
    void backStagePassesIncrease() {
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 100, 0),
            new Item("Backstage passes to a TAFKAL80ETC concert", 11, 0),
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 0),
            new Item("Backstage passes to a TAFKAL80ETC concert", 6, 0),
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 0),
            new Item("Backstage passes to a IDK concert", 1, 0),
            new Item("Backstage passes to a Robbie Williams concert", 0, 50),
            new Item("Backstage passes to a Hank Williams concert", -1, 50),
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, 1);
        assertEquals(app.items[1].quality, 1);
        assertEquals(app.items[2].quality, 2);
        assertEquals(app.items[3].quality, 2);
        assertEquals(app.items[4].quality, 3);
        assertEquals(app.items[5].quality, 3);
        assertEquals(app.items[6].quality, 0);
        assertEquals(app.items[7].quality, 0);
    }

    @Test
    void conjuredItems() {
        Item[] items = new Item[]{
            new Item("Conjured item 1", 100, 30),
            new Item("Conjured item 2", 0, 30),
            new Item("Conjured item 3", 0, 4),
            new Item("Conjured item 3", 0, 3)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(app.items[0].quality, 28);
        assertEquals(app.items[1].quality, 26);
        assertEquals(app.items[2].quality, 0);
        assertEquals(app.items[3].quality, 0);
    }
}
