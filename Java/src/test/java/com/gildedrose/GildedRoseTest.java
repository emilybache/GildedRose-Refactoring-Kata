package com.gildedrose;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @ParameterizedTest
    @CsvSource(value = {"3,2,0,1", "0,-1,3,5", "-1,-2,49,50"})
    void testAgedBrie(int sellIn, int sellInResult, int quality, int qualityResult) {
        Item[] items = new Item[] { new Item("Aged Brie", sellIn, quality) };
        GildedRose app = new GildedRose(items);

        app.updateQuality();
        assertEquals("Aged Brie", app.items[0].name);
        assertEquals(sellInResult, app.items[0].sellIn);
        assertEquals(qualityResult, app.items[0].quality);
    }

    @ParameterizedTest
    @CsvSource(value = {"11,10,25,26", "10,9,25,27", "5,4,25,28", "5,4,48,50", "0,-1,48,0"})
    void testBackstagePasses(int sellIn, int sellInResult, int quality, int qualityResult) {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", sellIn, quality) };
        GildedRose app = new GildedRose(items);

        app.updateQuality();
        assertEquals("Backstage passes to a TAFKAL80ETC concert", app.items[0].name);
        assertEquals(sellInResult, app.items[0].sellIn);
        assertEquals(qualityResult, app.items[0].quality);
    }

    @ParameterizedTest
    @CsvSource(value = {"1,1,80,80", "0,0,80,80", "-1,-1,80,80"})
    void testSulfuras(int sellIn, int sellInResult, int quality, int qualityResult) {
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", sellIn, quality)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();
        assertEquals("Sulfuras, Hand of Ragnaros", app.items[0].name);
        assertEquals(sellInResult, app.items[0].sellIn);
        assertEquals(qualityResult, app.items[0].quality);
    }

    @ParameterizedTest
    @CsvSource(value = {"2,1,5,4", "1,0,5,4", "0,-1,5,3", "0,-1,1,0"})
    void testRegularItem(int sellIn, int sellInResult, int quality, int qualityResult) {
        Item[] items = new Item[]{new Item("Regular item", sellIn, quality)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();
        assertEquals("Regular item", app.items[0].name);
        assertEquals(sellInResult, app.items[0].sellIn);
        assertEquals(qualityResult, app.items[0].quality);
    }

    @ParameterizedTest
    @CsvSource(value = {"2,1,5,3", "1,0,5,3", "0,-1,5,1", "0,-1,3,0"})
    void testConjured(int sellIn, int sellInResult, int quality, int qualityResult) {
        Item[] items = new Item[]{new Item("Conjured Mana Cake", sellIn, quality)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();
        assertEquals("Conjured Mana Cake", app.items[0].name);
        assertEquals(sellInResult, app.items[0].sellIn);
        assertEquals(qualityResult, app.items[0].quality);
    }
}
