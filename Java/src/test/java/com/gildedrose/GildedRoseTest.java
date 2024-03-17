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

    void should_decrease_sellin_and_quality_values_each_day() {
        int sellinStart = 6;
        int qualityStart = 8;
        final Item standardItem = new Item("Some item", sellinStart, qualityStart);
        GildedRose gildedRose = new GildedRose(new Item[] {standardItem});

        gildedRose.updateQuality();

        assertEquals(standardItem.sellIn,sellinStart - 1);
        assertEquals(standardItem.quality, qualityStart - 1);
    }
}
