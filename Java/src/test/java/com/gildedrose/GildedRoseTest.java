package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    public void lowers_sellIn_date_of_regular_items_each_day() throws Exception {
        Item[] items = new Item[] { new Item("+3 Strength Belt", 3, 10) };
        GildedRose app = new GildedRose(items);

        int originalSellIn = items[0].sellIn;
        int expectedSellIn = originalSellIn - 1;

        app.updateQuality();

        assertEquals(expectedSellIn, app.items[0].sellIn);
    }

    @Test
    public void lowers_quality_of_regular_items_each_day() throws Exception {
        Item[] items = new Item[] { new Item("+4 Constitution Boots", 1, 10) };
        GildedRose app = new GildedRose(items);

        int originalQuality = items[0].quality;
        int expectedQuality = originalQuality - 1;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void lowers_quality_of_conjured_items_faster_each_day() throws Exception {
        Item[] items = new Item[] { new Item("Conjured HP Pie", 1, 10) };
        GildedRose app = new GildedRose(items);

        int originalQuality = items[0].quality;
        int expectedQuality = originalQuality - 2;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void increases_quality_of_non_degrading_items_each_day() throws Exception {
        Item[] items = new Item[] { new Item("Aged Brie", 1, 10) };
        GildedRose app = new GildedRose(items);

        int originalQuality = items[0].quality;
        int expectedQuality = originalQuality + 1;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void decreases_quality_of_regular_items_twice_as_fast_after_sellIn_date_each_day() throws Exception {
        Item[] items = new Item[] { new Item("Bramble Cloak", -1, 10) };
        GildedRose app = new GildedRose(items);

        int originalQuality = items[0].quality;
        int expectedQuality = originalQuality - 2;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void decreases_quality_of_conjured_items_twice_as_fast_after_sellIn_date_each_day() throws Exception {
        Item[] items = new Item[] { new Item("Conjured Mana Cake", -1, 10) };
        GildedRose app = new GildedRose(items);

        int originalQuality = items[0].quality;
        int expectedQuality = originalQuality - 4;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void increases_quality_of_non_degrading_items_twice_as_fast_after_sellIn_date_each_day() throws Exception {
        Item[] items = new Item[] { new Item("Aged Brie", -1, 10) };
        GildedRose app = new GildedRose(items);

        int originalQuality = items[0].quality;
        int expectedQuality = originalQuality + 2;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void increases_quality_of_backstage_passes_each_day() throws Exception {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 20, 10) };
        GildedRose app = new GildedRose(items);

        int originalQuality = items[0].quality;
        int expectedQuality = originalQuality + 1;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void increases_quality_of_backstage_passes_by_2_from_10_days_before_concert() throws Exception {
        Item[] items = new Item[] { new Item("Backstage passes to a Royal Jelly concert", 10, 10) };
        GildedRose app = new GildedRose(items);

        int originalQuality = items[0].quality;
        int expectedQuality = originalQuality + 2;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void increases_quality_of_backstage_passes_by_3_from_5_days_before_concert() throws Exception {
        Item[] items = new Item[] { new Item("Backstage passes to a Medieval Tree concert", 5, 10) };
        GildedRose app = new GildedRose(items);

        int originalQuality = items[0].quality;
        int expectedQuality = originalQuality + 3;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void quality_cannot_be_negative() throws Exception {
        Item[] items = new Item[] { new Item("Necklace of Fireballs", 10, 0) };
        GildedRose app = new GildedRose(items);

        int expectedQuality = items[0].quality;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void quality_cannot_be_above_50_for_non_legendary_and_non_degradable_items() throws Exception {
        Item[] items = new Item[] { new Item("Aged Brie", 10, 50) };
        GildedRose app = new GildedRose(items);

        int expectedQuality = items[0].quality;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void quality_cannot_be_above_50_for_backstage_passes() throws Exception {
        Item[] items = new Item[] { new Item("Backstage passes to a Fine Adventurer concert", 10, 50) };
        GildedRose app = new GildedRose(items);

        int expectedQuality = items[0].quality;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void quality_cannot_be_above_50_for_non_legendary_and_non_degradable_items_past_inSell_date() throws Exception {
        Item[] items = new Item[] { new Item("Aged Brie", -1, 49) };
        GildedRose app = new GildedRose(items);

        int originalQuality = items[0].quality;
        int expectedQuality = originalQuality + 1;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void quality_is_0_for_backstage_passes_past_sellIn_date() throws Exception {
        Item[] items = new Item[] { new Item("Backstage passes to a Royal Innkeeper concert", 0, 50) };
        GildedRose app = new GildedRose(items);

        int expectedQuality = 0;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void quality_is_always_80_for_legendary_items() throws Exception {
        Item[] items = new Item[] { new Item("Sulfuras, Hand of Ragnaros", 3, 80) };
        GildedRose app = new GildedRose(items);

        int expectedQuality = items[0].quality;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }

    @Test
    public void quality_is_always_80_for_legendary_items_past_sellIn_date() throws Exception {
        Item[] items = new Item[] { new Item("Sulfuras, Hand of Ragnaros", -3, 80) };
        GildedRose app = new GildedRose(items);

        int expectedQuality = items[0].quality;

        app.updateQuality();

        assertEquals(expectedQuality, app.items[0].quality);
    }
}
