package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateStorage();
        assertEquals("foo", app.items[0].name);
    }

    @Test
    void standardItem_quality_decreases_sellin_decreases_each_day() {
        int startingSellin = 10;
        int startingQuality = 14;
        final Item standardItem = new Item("spam", startingSellin, startingQuality);
        GildedRose app = new GildedRose(new Item[]{standardItem});

        app.updateStorage();

        assertThat(standardItem.sellIn).isEqualTo(startingSellin - 1);
        assertThat(standardItem.quality).isEqualTo(startingQuality - 1);
    }

    @Test
    void multiple_items_degrade_each_day() {
        Item firstItem = new Item("First Standard Item", 5, 4);
        Item secondItem = new Item("Second Standard Item", 3, 2);
        GildedRose app = new GildedRose(new Item[]{firstItem, secondItem});

        app.updateStorage();

        assertThat(firstItem.sellIn).isEqualTo(4);
        assertThat(firstItem.quality).isEqualTo(3);
        assertThat(secondItem.sellIn).isEqualTo(2);
        assertThat(secondItem.quality).isEqualTo(1);
    }

    @Test
    void item_quality_degrades_twice_as_fast_past_sellin_date() {
        Item item = new Item("Standard Item", -1, 6);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isEqualTo(4);
    }

    @Test
    void item_quality_degrades_by_one_with_one_day_left() {
        Item item = new Item("Standard Item", 1, 6);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isEqualTo(5);
    }

    @Test
    void item_quality_degrades_down_to_zero() {
        Item item = new Item("Standard Item", 5, 1);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isZero();
    }

    @Test
    void item_quality_is_never_negative() {
        Item item = new Item("First Standard Item", 4, 0);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isZero();
    }

    @Test
    void aged_items_increase_in_quality_over_time() {
        Item item = new Item("Aged Brie", 5, 6);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isEqualTo(7);
    }

    @Test
    void aged_item_quality_49_increases_up_to_50() {
        Item item = new Item("Aged Brie", 5, 49);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isEqualTo(50);
    }

    @Test
    void aged_item_quality_increases_twice_as_fast_past_sellin_date() {
        Item item = new Item("Aged Brie", 0, 6);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isEqualTo(8);
    }

    @Test
    void aged_item_quality_50_past_sellin_date_does_not_increase() {
        Item item = new Item("Aged Brie", 0, 50);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isEqualTo(50);
    }

    @Test
    void quality_of_an_item_is_never_greater_than_50() {
        Item item = new Item("Aged Brie", 50, 100);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isEqualTo(100);
    }

    @Test
    void legendary_items_never_have_to_be_sold() {
        Item item = new Item("Sulfuras, Hand of Ragnaros", -1, 100);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.sellIn).isEqualTo(-1);
    }

    @Test
    void conjured_items_degrade_in_quality_twice_as_fast() {
        Item item = new Item("Conjured", 10, 10);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isEqualTo(8);
        assertThat(item.sellIn).isEqualTo(9);
    }

    @Test
    void conjured_item_degrade_in_quality_by_four_if_expired(){
        Item item = new Item("Conjured", 0, 10);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isEqualTo(6);
    }
    @Test
    void conjured_item_quality_can_never_be_negative(){
        Item item = new Item("Conjured", 10, 1);
        GildedRose app = new GildedRose(new Item[]{item});

        app.updateStorage();

        assertThat(item.quality).isEqualTo(0);
    }
}
