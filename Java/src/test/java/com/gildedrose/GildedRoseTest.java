package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }

    @Test
    void standardItem_quality_decreases_sellin_decreases_each_day() {
        int startingSellin = 10;
        int startingQuality = 14;
        final Item standardItem = new Item("spam", startingSellin, startingQuality);
        GildedRose app = new GildedRose(new Item[]{standardItem});

        app.updateQuality();

        assertThat(standardItem.sellIn).isEqualTo(startingSellin - 1);
        assertThat(standardItem.quality).isEqualTo(startingQuality - 1);
    }

    @Test
    void multiple_items_degrade_each_day() {
        Item firstItem = new Item("First Standard Item", 5, 4);
        Item secondItem = new Item("Second Standard Item", 3, 2);
        GildedRose app = new GildedRose(new Item[]{firstItem, secondItem});

        app.updateQuality();

        assertThat(firstItem.sellIn).isEqualTo(4);
        assertThat(firstItem.quality).isEqualTo(3);
        assertThat(secondItem.sellIn).isEqualTo(2);
        assertThat(secondItem.quality).isEqualTo(1);
    }
}
