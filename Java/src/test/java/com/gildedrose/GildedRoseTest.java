package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void shouldDefaultDecreaseQualityBeforeSellIn() {
        Item[] items = new Item[]{
            new Item("testProductName", 9, 30)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("testProductName", app.items[0].name);
        assertEquals(29, app.items[0].quality);
    }

    @Test
    void shouldDoubleDecreaseQualityAfterSellIn() {
        Item[] items = new Item[]{
            new Item("testProductName", -2, 30)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("testProductName", app.items[0].name);
        assertEquals(28, app.items[0].quality);
    }

    @Test
    void shouldNotDecreaseQualityBelowZero() {
        Item[] items = new Item[]{
            new Item("testProductName", 9, 0)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("testProductName", app.items[0].name);
        assertEquals(0, app.items[0].quality);
    }

    @Test
    void shouldIncreaseQualityOfAgedBrie() {
        Item[] items = new Item[]{
            new Item("Aged Brie", 9, 30)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Aged Brie", app.items[0].name);
        assertEquals(31, app.items[0].quality);
    }

    @Test
    void shouldNotIncreaseQualityAboveFifty() {
        Item[] items = new Item[]{
            new Item("Aged Brie", 9, 50)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Aged Brie", app.items[0].name);
        assertEquals(50, app.items[0].quality);
    }

    @Test
    void shouldNotChangeSulfurasQuality() {
        Item[] items = new Item[]{
            new Item("Sulfuras, Hand of Ragnaros", 9, 80)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Sulfuras, Hand of Ragnaros", app.items[0].name);
        assertEquals(80, app.items[0].quality);
    }

    @Test
    void shouldIncreaseQualityBy2Below10DaysForSpecialProducts() {
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 9, 32)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Backstage passes to a TAFKAL80ETC concert", app.items[0].name);
        assertEquals(34, app.items[0].quality);
    }

    @Test
    void shouldIncreaseQualityBy3AfterBelow5DaysForSpecialProducts() {
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 4, 32)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Backstage passes to a TAFKAL80ETC concert", app.items[0].name);
        assertEquals(35, app.items[0].quality);
    }

    @Test
    void shouldSetZeroQualityForSpecialProductsAfterSellIn() {
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", -2, 32)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Backstage passes to a TAFKAL80ETC concert", app.items[0].name);
        assertEquals(0, app.items[0].quality);
    }

    @Test
    void shouldDoubleDecreaseQualityForConjuredItems() {
        Item[] items = new Item[]{
            new Item("Conjured", 9, 30)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Conjured", app.items[0].name);
        assertEquals(28, app.items[0].quality);
    }

    @Test
    void shouldNotExceedQualityLimitForBackstageItem() {
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 4, 49)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(50, app.items[0].quality);
    }

}
