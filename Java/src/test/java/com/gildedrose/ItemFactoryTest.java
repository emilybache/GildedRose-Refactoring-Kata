package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ItemFactoryTest {

    @Test
    void givenAgedBrieName_whenCreateItem_thenReturnsAgedBrieItem() {
        Item item = ItemFactory.createItem("Aged Brie", 10, 20);
        assertTrue(item instanceof AgedBrieItem);
        assertEquals("Aged Brie", item.getName());
        assertEquals(10, item.sellIn);
        assertEquals(20, item.quality);
    }

    @Test
    void givenBackstagePassName_whenCreateItem_thenReturnsBackstagePassItem() {
        Item item = ItemFactory.createItem("Backstage passes to a TAFKAL80ETC concert", 15, 30);
        assertTrue(item instanceof BackstagePassItem);
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.getName());
        assertEquals(15, item.sellIn);
        assertEquals(30, item.quality);
    }

    @Test
    void givenSulfurasName_whenCreateItem_thenReturnsSulfurasItem() {
        Item item = ItemFactory.createItem("Sulfuras, Hand of Ragnaros", 0, 80);
        assertTrue(item instanceof SulfurasItem);
        assertEquals("Sulfuras, Hand of Ragnaros", item.getName());
        assertEquals(0, item.sellIn);
        assertEquals(80, item.quality);
    }

    @Test
    void givenNormalItemName_whenCreateItem_thenReturnsNormalItem() {
        Item item = ItemFactory.createItem("Some Normal Item", 5, 10);
        assertTrue(item instanceof NormalItem);
        assertEquals("Some Normal Item", item.getName());
        assertEquals(5, item.sellIn);
        assertEquals(10, item.quality);
    }

    @Test
    void givenUnknownItemName_whenCreateItem_thenReturnsNormalItem() {
        Item item = ItemFactory.createItem("Unknown Item", 5, 10);
        assertTrue(item instanceof NormalItem);
        assertEquals("Unknown Item", item.getName());
        assertEquals(5, item.sellIn);
        assertEquals(10, item.quality);
    }
}
