package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("fixme", app.items[0].name);
    }
    
    // copy-paste from https://gist.github.com/adelatorrefoss/ebc658b3e0054913dced80c8fe384de0
    @Test
    public void quality_never_is_negative() {
        Item[] items = new Item[]{new Item("foo", 0, 0)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(0, app.items[0].quality);
    }

    @Test
    public void sulfuras_could_not_be_sold() {
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", 10, 0)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(10, app.items[0].sellIn);
    }

    @Test
    public void sulfuras_could_not_decrease_quality() {
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", 10, 10)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(10, app.items[0].quality);
    }

    @Test
    public void quality_could_not_be_more_than_fifty() {
        Item[] items = new Item[]{new Item("Aged Brie", 10, 50)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(50, app.items[0].quality);
    }

    @Test
    public void item_with_date_passed_quality_decrease_by_twice() {
        Item[] items = new Item[]{new Item("foo", -1, 40)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(38, app.items[0].quality);
    }

    @Test
    public void aged_brie_increase_quality_when_it_gets_older() {
        Item[] items = new Item[]{new Item("Aged Brie", 1, 40)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(41, app.items[0].quality);
    }

    @Test
    public void aged_brie_increase_by_two_quality_when_date_passed() {
        Item[] items = new Item[]{new Item("Aged Brie", -1, 40)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(42, app.items[0].quality);
    }

    @Test
    public void aged_brie_increase_by_two_quality_when_date_passed_and_not_more_than_fifty() {
        Item[] items = new Item[]{new Item("Aged Brie", -1, 50)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(50, app.items[0].quality);
    }

    @Test
    public void backstage_passes_increase_quality_by_two_when_sellin_less_than_ten() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, 40)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(42, app.items[0].quality);
    }

    @Test
    public void backstage_passes_increase_quality_by_two_when_sellin_less_than_six() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 6, 40)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(42, app.items[0].quality);
    }

    @Test
    public void backstage_passes_increase_quality_by_three_when_sellin_less_than_five() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 5, 40)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(43, app.items[0].quality);
    }

    @Test
    public void backstage_passes_increase_quality_by_two_when_sellin_less_than_six_and_not_more_than_fifty() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 6, 49)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(50, app.items[0].quality);
    }

    @Test
    public void backstage_passes_increase_quality_by_three_when_sellin_less_than_five_and_not_more_than_fifty() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 5, 48)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(50, app.items[0].quality);
    }

    @Test
    public void backstage_passes_quality_drops_to_zero_after_concert() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 0, 40)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(0, app.items[0].quality);
    }

    @Test
    public void backstage_passes_quality_increase_quality_by_one_when_date_is_more_than_10() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 11, 40)};
        GildedRose app = new GildedRose(items);

        app.updateQuality();

        assertEquals(41, app.items[0].quality);
    }

}
