package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void should_never_be_greater_than_50_quality_of_an_item() {
        Item item = new Item("Aged Brie", 5, 50);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 50);
    }


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

    // I should maybe delete this
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
    void should_increases_up_to_50_when_aged_item_quality_is_49() {
        Item item = new Item("Aged Brie", 5, 49);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 50);
    }

    @Test
    void should_decrease_item_quality_down_to_0() {
        Item item = new Item("Standard Item", 4, 1);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 0);
    }


    @Test
    void aged_item_quality_increases_twice_as_fast_past_sellin_date() {
        Item item = new Item("Aged Brie", 0, 6);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 8);
    }

    @Test
    void aged_item_quality_50_past_sellin_date_does_not_increase() {
        Item item = new Item("Aged Brie", 0, 50);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 50);
    }

    @Test
    void legendary_items_never_have_to_be_sold() {
        Item item = new Item("Sulfuras, Hand of Ragnaros", -1, 80);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.sellIn, -1);
    }

    @Test
    void legendary_items_never_decrease_in_quality() {
        Item item = new Item("Sulfuras, Hand of Ragnaros", -1, 80);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 80);
    }


    @Test
    void backstage_passes_increase_in_quality_as_sellIn_date_approaches() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 21);
    }

    @Test
    void backstage_passes_increase_in_quality_by_1_when_there_are_10_days_or_less() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 11, 48);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 49);
    }

    @Test
    void backstage_passes_increase_in_quality_by_2_when_there_are_10_days_or_less() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 20);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 22);
    }

    @Test
    void backstage_passes_quality_49_increase_up_to_50_when_there_are_10_days_or_less() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality,50);
    }

    @Test
    void backstage_passes_increase_in_quality_by_2_when_there_are_6_days_or_less() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 6, 46);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 48);
    }

    @Test
    void backstage_passes_increase_in_quality_by_3_when_there_are_5_days_or_less() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 20);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 23);
    }

    @Test
    void backstage_passes_quality_47_increase_up_to_50_when_there_are_5_days_or_less() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 47);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 50);
    }

    @Test
    void backstage_passes_quality_49_increase_up_to_50_when_there_are_5_days_or_less() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 50);
    }

    @Test
    void backstage_passes_quality_is_0_after_concert() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 0, 20);
        GildedRose gildedRose = new GildedRose(new Item[] { item });

        gildedRose.updateQuality();

        assertEquals(item.quality, 0);
    }
}
