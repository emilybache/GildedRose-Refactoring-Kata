package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {
    @Test
    void qualityNeverNegative() {
        Item[] items = new Item[] { new Item("fixme", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(0, app.items[0].quality);
    }

    @Test
    void qualityDegrades2xAfterSellByDate() {
        Item[] items = new Item[] { new Item("fixme", 1, 10) };
        GildedRose app = new GildedRose(items);

        // check that quality only goes down 1 while sellIn > 0
        app.updateQuality();
        assertEquals(9, app.items[0].quality);

        // check that quality goes down 2 while sellIn <= 0
        app.updateQuality(); // <- sellIn is now 0
        assertEquals(7, app.items[0].quality);
    }

    @Test
    void qualityIncreasesForAgedBrie() {
        Item[] items = new Item[] { new Item("Aged Brie", 1, 0) };
        GildedRose app = new GildedRose(items);

        // NOTE: aged brie increases twice as fast after sellIn days have elapsed

        // quality should go up 1x speed
        app.updateQuality();
        assertEquals(1, app.items[0].quality);

        // quality should go up 2x because sellIn is 0
        app.updateQuality();
        assertEquals(3, app.items[0].quality);
    }

    @Test
    void qualityNeverPasses50() {
        // aged brie is the only item that increases in quality (besides the pass)
        Item[] items = new Item[] { new Item("Aged Brie", 1, 49) };
        GildedRose app = new GildedRose(items);

        // should go up 1 to 50
        app.updateQuality();
        assertEquals(50, app.items[0].quality);

        // should stop at 50
        app.updateQuality();
        assertEquals(50, app.items[0].quality);
    }

    @Test
    void legendaryItemsBehaveAsExpected() {
        Item[] items = new Item[] { new Item("Sulfuras, Hand of Ragnaros", 0, 80) };
        GildedRose app = new GildedRose(items);

        app.updateQuality();
        assertEquals(80, app.items[0].quality);
    }

    @Test
    void backstagePassesBehaveAsExpected() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 10, 5) };
        GildedRose app = new GildedRose(items);

        // 10 days or less -> grows by 2
        app.updateQuality(); // sellIn 9
        assertEquals(7, app.items[0].quality);

        app.updateQuality(); // sellIn 8, quality = 9
        app.updateQuality(); // sellIn 7, quality = 11
        app.updateQuality(); // sellIn 6, quality = 13

        // updateQuality
        //// it will update quality
        //// it will subtract 1 to sellIn
        //// TODO: make this clearer when we refactor the actual code

        app.updateQuality(); // sellIn 5, quality = 16 <- is not the case, sellIn 5, quality is 15

        // It changes quality first, and then decreases sellIn
        assertEquals(15, app.items[0].quality);

        // -1 days -> quality is 0
        app.updateQuality(); // sellIn 4
        app.updateQuality(); // sellIn 3
        app.updateQuality(); // sellIn 2
        app.updateQuality(); // sellIn 1
        app.updateQuality(); // sellIn 0
        app.updateQuality(); // sellIn -1

        assertEquals(0, app.items[0].quality);
    }

    @Test
    void conjuredItemsDegrade2x() {
        Item[] items = new Item[] { new Item("Conjured Brie", 1, 50) };
        GildedRose app = new GildedRose(items);

        // twice as fast as normal degrade = -2
        app.updateQuality();
        assertEquals(48, app.items[0].quality);

        // twice as fast as sellIn has passed = -4
        app.updateQuality();
        assertEquals(44, app.items[0].quality);
    }
}
