package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[]{new Item("foo", 0, 0)};
        GildedRose app = new GildedRose(items);
        GildedRose.updateQuality(app);
        assertEquals("foo", app.items[0].name);
    }

    @Test
    void degradesTwiceAsFastAfterSellInDateHasPassed() {
        Item[] items = new Item[]{new Item("foo", 0, 2)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(0, items[0].quality);
    }

    @Test
    void neverMustHaveNegativeQuality() {
        Item[] items = new Item[]{new Item("foo", 0, 0)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(0, items[0].quality);
    }

    @Test
    void increaseAgedBrieQualityWhenItGetsOlder() {
        Item[] items = new Item[]{new Item("Aged Brie", 0, 2)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(4, items[0].quality);
    }

    @Test
    void neverMustHaveAnItemWithMoreThan50OfQuality() {
        Item[] items = new Item[]{new Item("Aged Brie", 0, 50)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(50, items[0].quality);
    }


    @Test
    void neverModifySulfurasQuality() {
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", 0, 50)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(50, items[0].quality);
    }

    @Test
    void neverModifySulfurasQualityEvenISGreaterThan50() {
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", 0, 80)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(80, items[0].quality);
    }

    @Test
    void mustIncreaseBackstagePassesQualityWhenItsSellInApproaches() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 20),
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 20)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(21, items[0].quality);
        assertEquals(22, items[1].quality);
        assertEquals(23, items[2].quality);
    }
}
