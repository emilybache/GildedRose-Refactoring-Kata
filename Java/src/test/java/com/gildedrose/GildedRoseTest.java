package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void should_decrease_multiple_items_quality_each_day() {
        Item firstItem = new Item("First Standard Item", 5, 4);
        Item secondItem = new Item("Second Standard Item", 3, 2);
        GildedRose gildedRose = new GildedRose(new Item[] { firstItem, secondItem });

        gildedRose.updateQuality();

        assertEquals(firstItem.sellIn, 4);
        assertEquals(firstItem.quality, 3);
        assertEquals(secondItem.sellIn, 2);
        assertEquals(secondItem.quality, 1);
    }

    @Test
    void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }


    @Test
    void should_degrade_quality_by_one_with_1_day_left() {
        Item item = new Item("New standard item: testing how quality degrades by on with 1 day left", 1, 4);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 3);
    }


    @Test
    void should_decrease_sellin_and_quality_values_each_day() {
        int sellinStart = 6;
        int qualityStart = 8;
        final Item standardItem = new Item("Some item", sellinStart, qualityStart);
        GildedRose gildedRose = new GildedRose(new Item[] {standardItem});

        gildedRose.updateQuality();

        assertEquals(standardItem.sellIn,sellinStart - 1);
        assertEquals(standardItem.quality, qualityStart - 1);
    }


    @Test
    void should_increase_aged_brie_in_quality_over_time() {
        Item item = new Item("Aged Brie", 5, 6);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 7);
    }

    @Test
    void should_never_be_never_negative_item_quality() {
        Item item = new Item("First Standard Item", 4, 0);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 0);
    }

    @Test
    void should_decrease__twice_as_fast_past_sellin_date_item_quality() {
        Item item = new Item("A new standard Item to be decreased twice as fast ", -1, 4);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 2);
    }

    @Test
    void should_decrease_item_quality_down_to_0() {
        Item item = new Item("Standard Item", 4, 1);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 0);
    }
}
