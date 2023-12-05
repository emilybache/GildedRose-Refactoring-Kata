package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

class GildedRoseTest {

    @Test
    void updateQuality_standardItem_normalStep() {
        GildedRose app = createApp(createItem("standard", 10, 15));
        app.updateQuality();
        assertEquals(9, app.items[0].sellIn);
        assertEquals(14, app.items[0].quality);
    }

    @Test
    void updateQuality_standardItem_sellInTo0() {
        GildedRose app = createApp(createItem("standard", 1, 15));
        app.updateQuality();
        assertEquals(0, app.items[0].sellIn);
        assertEquals(14, app.items[0].quality);
    }

    @Test
    void updateQuality_standardItem_minQualityO() {
        GildedRose app = createApp(createItem("standard", 1, 0));
        app.updateQuality();
        assertEquals(0, app.items[0].sellIn);
        assertEquals(0, app.items[0].quality);
    }

    @Test
    void updateQuality_standardItem_doubleDecayAfterLastSellDate() {
        GildedRose app = createApp(createItem("standard", 0, 10));
        app.updateQuality();
        assertEquals(-1, app.items[0].sellIn);
        assertEquals(8, app.items[0].quality);
    }

    @Test
    void updateQuality_standardItem_maxQuality50() {
        GildedRose app = createApp(createItem("standard", 10, 60));
        app.updateQuality();

        assertEquals(49, app.items[0].quality);
    }

    @Test
    void updateQuality_legendaryItem() {
        GildedRose app = createApp(createItem("Sulfuras, Hand of Ragnaros ", 10, 80));
        app.updateQuality();

        assertEquals(10, app.items[0].sellIn);
        assertEquals(80, app.items[0].quality);
    }

    @Test
    void updateQuality_agingItem() {
        fail("NYI");
    }

    @Test
    void updateQuality_conjuredItem() {
        fail("NYI");
    }

    private GildedRose createApp(Item[] items) {
        return new GildedRose(items);
    }

    private Item[] createItem(String name, int sellIn, int quality) {
        return new Item[] { new Item(name, sellIn, quality) };
    }
}
