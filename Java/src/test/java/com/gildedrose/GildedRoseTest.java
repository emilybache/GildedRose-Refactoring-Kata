package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

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
        GildedRose app = createApp(createItem("SulFUras, Hand of Ragnaros ", 10, 80));
        app.updateQuality();

        assertEquals(10, app.items[0].sellIn);
        assertEquals(80, app.items[0].quality);
    }

    @Test
    void updateQuality_agingItem_normalIncrease() {
        GildedRose app = createApp(createItem("Aging Brie", 20, 10));
        app.updateQuality();

        assertEquals(19, app.items[0].sellIn);
        assertEquals(11, app.items[0].quality);
    }

    @Test
    void updateQuality_agingItem_doubleQualityIncrease() {
        GildedRose app = createApp(createItem("Aging Brie", 11, 10));
        app.updateQuality();

        assertEquals(10, app.items[0].sellIn);
        assertEquals(12, app.items[0].quality);
    }

    @Test
    void updateQuality_agingItem_doubleTripleIncrease() {
        GildedRose app = createApp(createItem("A lot of Aging BRIE", 6, 10));
        app.updateQuality();

        assertEquals(5, app.items[0].sellIn);
        assertEquals(13, app.items[0].quality);
    }

    @Test
    void updateQuality_agingItem_normalDecrease() {
        GildedRose app = createApp(createItem("Aging BRIE", 0, 50));
        app.updateQuality();

        assertEquals(-1, app.items[0].sellIn);
        assertEquals(49, app.items[0].quality);
    }

    @Test
    void updateQuality_agingItem_BackstagePasses_HardDegradation() {
        GildedRose app = createApp(createItem("Backstage paSSes for", 0, 50));
        app.updateQuality();

        assertEquals(-1, app.items[0].sellIn);
        assertEquals(0, app.items[0].quality);
    }

    @Test
    void updateQuality_conjuredItem_normalStep() {
        GildedRose app = createApp(createItem("Conjured stuff", 10, 15));
        app.updateQuality();

        assertEquals(9, app.items[0].sellIn);
        assertEquals(13, app.items[0].quality);
    }

    @Test
    void updateQuality_conjuredItem_doubleDecayAfterLastSellDate() {
        GildedRose app = createApp(createItem("Conjured stuff", 0, 15));
        app.updateQuality();

        assertEquals(-1, app.items[0].sellIn);
        assertEquals(11, app.items[0].quality);
    }

    private GildedRose createApp(Item[] items) {
        return new GildedRose(items);
    }

    private Item[] createItem(String name, int sellIn, int quality) {
        return new Item[] { new Item(name, sellIn, quality) };
    }
}
