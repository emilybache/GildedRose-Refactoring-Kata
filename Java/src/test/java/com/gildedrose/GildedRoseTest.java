package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class GildedRoseTest {
    @Test
    void normalItemDegradesQuality() {
        Item item = new Item("Normal Item", 10, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(9, item.sellIn);
        assertEquals(19, item.quality);
    }

    @Test
    void normalItemDegradesTwiceAfterSellIn() {
        Item item = new Item("Normal Item", 0, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(-1, item.sellIn);
        assertEquals(18, item.quality);
    }

    @Test
    void agedBrieIncreasesQuality() {
        Item item = new Item("Aged Brie", 2, 0);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(1, item.sellIn);
        assertEquals(1, item.quality);
    }

    @Test
    void agedBrieIncreasesQualityTwiceAfterSellIn() {
        Item item = new Item("Aged Brie", 0, 0);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(-1, item.sellIn);
        assertEquals(2, item.quality);
    }

    @Test
    void qualityNeverMoreThanFifty() {
        Item item = new Item("Aged Brie", 1, 50);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(0, item.sellIn);
        assertEquals(50, item.quality);
    }

    @Test
    void sulfurasNeverChanges() {
        Item item = new Item("Sulfuras, Hand of Ragnaros", 0, 80);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(0, item.sellIn);
        assertEquals(80, item.quality);
    }

    @Test
    void backstagePassIncreasesByOneOverTenDays() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(14, item.sellIn);
        assertEquals(21, item.quality);
    }

    @Test
    void backstagePassIncreasesByTwoTenToSixDays() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(9, item.sellIn);
        assertEquals(22, item.quality);
    }

    @Test
    void backstagePassIncreasesByThreeFiveToOneDays() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(4, item.sellIn);
        assertEquals(23, item.quality);
    }

    @Test
    void backstagePassDropsToZeroAfterConcert() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 0, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(-1, item.sellIn);
        assertEquals(0, item.quality);
    }

    @Test
    void conjuredItemDegradesTwiceAsFast() {
        Item item = new Item("Conjured Mana Cake", 3, 6);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(2, item.sellIn);
        assertEquals(4, item.quality);
    }

    @Test
    void conjuredItemDegradesTwiceAsFastAfterSellIn() {
        Item item = new Item("Conjured Mana Cake", 0, 6);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(-1, item.sellIn);
        assertEquals(2, item.quality);
    }

    @Test
    void qualityNeverNegative() {
        Item item = new Item("Normal Item", 5, 0);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(0, item.quality);
    }
}
