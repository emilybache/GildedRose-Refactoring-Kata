package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void aged_brie_increase_in_quality_by_1_when_quantity_less_than_50() {
        Item[] items = new Item[] { new Item("Aged Brie", 12, 48) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(49, app.items[0].quality);
    }
    
    @Test
    void aged_brie_increase_in_quality_by_twice_when__expired() {
        Item[] items = new Item[] { new Item("Aged Brie", -1, 40) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(42, app.items[0].quality);
    }
    
    @Test
    void backstage_passes_increase_in_quality_by_1_when_quality_lessthan_50() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 15, 28) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(29, app.items[0].quality);
    }
    
    
    @Test
    void backstage_passes_increase_in_quality_by_2_when_there_are_10_daysOrLess() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 6, 20) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(22, app.items[0].quality);
    }
    
    @Test
    void backstage_passes_increase_in_quality_by_3_when_there_are_5_daysOrLess() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 4, 20) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(23, app.items[0].quality);
    }
    
    @Test
    void backstage_passes_quality_decreases_to_0_when_expired() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", -1, 20) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(0, app.items[0].quality);
    }
    
    @Test
    void normal_items_decrease_in_quality_by_1_when_quality_morethan_0() {
        Item[] items = new Item[] { new Item("Normal Item", 5, 20) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(19, app.items[0].quality);
    }
    
    @Test
    void normal_items_decrease_in_quality_by_twice_when_expired() {
        Item[] items = new Item[] { new Item("Normal Item", -1, 20) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(18, app.items[0].quality);
    }
    @Test
    void conjured_items_decrease_in_quality_by_twice_compared_to_normal_items() {
        Item[] items = new Item[] { new Item("Conjured", 12, 20) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(18, app.items[0].quality);
    }
    
    @Test
    void conjured_items_decrease_in_quality_by_twice_compared_to_normal_items_when_expired() {
        Item[] items = new Item[] { new Item("Conjured", -1, 20) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(16, app.items[0].quality);
    }
    
    @Test
    void sell_in_decreses_by_1() {
        Item[] items = new Item[] { new Item("Conjured", 12, 20) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(11, app.items[0].sellIn);
    }
   
}
