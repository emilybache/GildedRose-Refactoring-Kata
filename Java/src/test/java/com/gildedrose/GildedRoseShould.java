package com.gildedrose;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GildedRoseShould {


    // unknown CHECK
    // sulfuras

    @Test
    void when_item_name_is_AgedBrie_and_quality_is_lt_50_then_should_decrease_sellIn_and_increase_quality_in_1() {
        Item[] items = {new Item("Aged Brie", 10, 40)};
        GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        assertEquals(9, gildedRose.items[0].sellIn);
        assertEquals(41, gildedRose.items[0].quality);
    }

    @Test
    void when_item_is_Backstage_and_sellIn_is_lt_6_and_quality_is_lt_48_then_should_increase_in_3_quality_and_decrease_sellIn_in_1() {
        Item[] items = {new Item("Backstage passes to a TAFKAL80ETC concert", 5, 47)};
        GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        assertEquals(4, gildedRose.items[0].sellIn);
        assertEquals(50, gildedRose.items[0].quality);
    }

    @Test
    void when_item_is_AgedBrie_and_quality_is_lt_49_and_sellin_is_lt_0_then_quality_should_increase_in_1_and_decrease_sellIn_in_1(){
        Item [] items = {new Item("Aged Brie",-1, 48)};

        GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        assertEquals(50, gildedRose.items[0].quality);
        assertEquals(-2, gildedRose.items[0].sellIn);

    }

    @Test
    void when_item_is_Backstage_and_sellIn_is_lt_0_then_quality_should_be_set_in_0_and_decreases_sellIn_in_1(){
        Item [] items = {new Item("Backstage passes to a TAFKAL80ETC concert",-1,999)};

        GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        assertEquals(0, gildedRose.items[0].quality);
        assertEquals(-2, gildedRose.items[0].sellIn);
    }



    // unknown CHECK
    @Test
    void when_item_name_is_unknown_and_quality_is_lt_0_and_sellin_is_gt_0_then_should_maintains_quality_and_decrease_sellin_1() {
        Item[] items = {new Item("Another Different Item", 10, -1)};
        GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        assertEquals(9, gildedRose.items[0].sellIn);
        assertEquals(-1, gildedRose.items[0].quality);
    }

    @Test
    void when_item_name_is_unknown_and_quality_is_gt_0_and_sellin_is_gt_0_then_should_decrease_quality_in_1_and_sellIn_in_1() {
        Item[] items = {new Item("Another Different Item", 10, 10)};
        GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        assertEquals(9, gildedRose.items[0].sellIn);
        assertEquals(9, gildedRose.items[0].quality);
    }

    @Test
    void when_item_is_unknown_and_quality_is_gt_0_and_sellin_is_lt_0__then_should_decrease_quality_in_2_and_decrease_sellIn_in_1(){
        Item [] items = new Item[]{new Item("Another Different Item", -1, 999)};

        GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        assertEquals(997, gildedRose.items[0].quality);
        assertEquals(-2, gildedRose.items[0].sellIn);
    }


}
