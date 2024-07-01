package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class GildedRoseTest {

    @Test
    void ElixirDepreciates() throws Exception{
        Item[] items = new Item[]{new Item("Elixir", 0, 40)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        app.updateQuality();
        assertEquals(-2, app.items[0].sellIn);
        assertEquals(36, app.items[0].quality);
    }

    // TODO Write test cases for each item. Check Edge cases.

    @Test
    void backStage_Pass_Expires_After_Concert() throws Exception {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 2, 15)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        app.updateQuality();
        app.updateQuality();
        assertEquals(-1, app.items[0].sellIn);
        assertEquals(0, app.items[0].quality);
    }

    @Test
    public void backStage_quality_increases_twiceAsFast_10daysOrLess_before_concert() throws Exception {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, 20)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(9, app.items[0].sellIn);
        assertEquals(22, app.items[0].quality);
    }

    @Test
    public void quality_Never_Below_zero() throws Exception {
        Item[] items = new Item[]{new Item("Bread", 10, 0)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(9, app.items[0].sellIn);
        assertEquals(0, app.items[0].quality);
    }

    @Test
    public void sulfuras_hasConstant_quality_and_sellIn() throws Exception {
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", 8, 30)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(8, app.items[0].sellIn);
        assertEquals(30, app.items[0].quality);
    }

    @Test
    public void aged_brie_quality_isMax50() throws Exception {
        Item[] items = new Item[]{new Item("Aged Brie", 10, 50)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(9, app.items[0].sellIn);
        assertEquals(50, app.items[0].quality);
    }

    @Test
    public void backStage_quality_isMax50() throws Exception {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, 50)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(9, app.items[0].sellIn);
        assertEquals(50, app.items[0].quality);
    }
    @Test
    public void anyItem_quality_isMax50() throws Exception {
        Item[] items = new Item[]{new Item("Bread", 10, 50)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(9, app.items[0].sellIn);
        assertEquals(49, app.items[0].quality);
    }

}
