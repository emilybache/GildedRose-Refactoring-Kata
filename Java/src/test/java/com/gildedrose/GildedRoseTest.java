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

    @Test
    void item_quality_degrades_twice_as_fast_past_sellin_date() {
        Item item = new Item("Standard Item", -1, 6);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateQuality();

        assertThat(item.quality).isEqualTo(4);
    }

    @Test
    void item_quality_degrades_by_one_with_one_day_left() {
        Item item = new Item("Standard Item", 1, 6);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateQuality();

        assertThat(item.quality).isEqualTo(5);
    }

    @Test
    void item_quality_degrades_down_to_zero() {
        Item item = new Item("Standard Item", 5, 1);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateQuality();

        assertThat(item.quality).isZero();
    }

    @Test
    void item_quality_is_never_negative() {
        Item item = new Item("First Standard Item", 4, 0);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateQuality();

        assertThat(item.quality).isZero();
    }
}
