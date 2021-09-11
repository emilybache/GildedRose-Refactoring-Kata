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
        Item[] items = new Item[]{new Item(Constants.AGED_BRIE, 0, 2)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(4, items[0].quality);
    }

    @Test
    void neverMustHaveAnItemWithMoreThan50OfQuality() {
        Item[] items = new Item[]{new Item(Constants.AGED_BRIE, 0, 50)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(50, items[0].quality);
    }


    @Test
    void neverModifySulfurasQuality() {
        Item[] items = new Item[]{new Item(Constants.SULFURAS, 0, 50)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(50, items[0].quality);
    }

    @Test
    void neverModifySulfurasQualityEvenISGreaterThan50() {
        Item[] items = new Item[]{new Item(Constants.SULFURAS, 0, 80)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(80, items[0].quality);
    }

    @Test
    void mustIncreaseBackstagePassesQualityWhenItsSellInApproaches() {
        Item[] items = new Item[]{new Item(Constants.BACKSTAGE, 15, 20),
            new Item(Constants.BACKSTAGE, 10, 20),
            new Item(Constants.BACKSTAGE, 5, 20)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(21, items[0].quality);
        assertEquals(22, items[1].quality);
        assertEquals(23, items[2].quality);
    }

    @Test
    void mustDecreaseQualityTwiceAsFastIfItemIsConjured() {
        Item[] items = new Item[]{new Item(Constants.CONJURED, 0, 20)};
        GildedRose.updateQuality(new GildedRose(items));
        assertEquals(16, items[0].quality);
    }
}
