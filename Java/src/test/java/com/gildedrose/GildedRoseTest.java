package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void givenItem_whenUpdated_thenSellInDecrements() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(-1, app.items[0].sellIn);
    }

    @Test
    void givenItem_whenUpdated_thenQualityDecrements() {
        Item[] items = new Item[] { new Item("foo", 10, 10) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(9, app.items[0].quality);
    }

    @Test
    void givenExpiredItem_whenUpdated_thenQualityDecrementsByTwo() {
        Item[] items = new Item[] { new Item("foo", 0, 10) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(8, app.items[0].quality);
    }

    @Test
    void givenItemWithZeroQuality_whenUpdated_thenQualityIsNeverNegative() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(0, app.items[0].quality);
    }

    @Test
    void givenAgedBrie_whenUpdated_thenQualityIncrements() {
        Item[] items = new Item[] { new Item("Aged Brie", 10, 10) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(11, app.items[0].quality);
    }

    @Test
    void givenAgedBrieWithQuality50_whenUpdated_thenQualityRemainsTheSame() {
        Item[] items = new Item[] { new Item("Aged Brie", 10, 50) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(50, app.items[0].quality);
    }

    @Test
    void givenLegendaryItem_whenUpdated_thenSellInStaysTheSame() {
        Item[] items = new Item[] { new Item("Sulfuras, Hand of Ragnaros", 0, 80) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(0, app.items[0].sellIn);
    }

    @Test
    void givenLegendaryItem_whenUpdated_thenQualityStaysTheSame() {
        Item[] items = new Item[] { new Item("Sulfuras, Hand of Ragnaros", 0, 80) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(80, app.items[0].quality);
    }

    @Test
    void givenBackstagePasses_whenUpdated_thenQualityIncrements() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 20, 10) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(11, app.items[0].quality);
    }

    @Test
    void givenBackstagePassesWith10DaysToConcert_whenUpdated_thenQualityIncrementsByTwo() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 10, 10) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(12, app.items[0].quality);
    }

    @Test
    void givenBackstagePassesWith5DaysToConcert_whenUpdated_thenQualityIncrementsByThree() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 5, 10) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(13, app.items[0].quality);
    }

    @Test
    void givenBackstagePassesAfterTheConcert_whenUpdated_thenQualityIsZero() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 0, 10) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(0, app.items[0].quality);
    }
}
