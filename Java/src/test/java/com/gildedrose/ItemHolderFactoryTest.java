package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ItemHolderFactoryTest {

    @Test
    void create_generic_item() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("foo", 0, 0));
        assertEquals("foo", itemHolder.item.name);
        assertEquals(0, itemHolder.item.sellIn);
        assertEquals(0, itemHolder.item.quality);
    }

    @Test
    void quality_decrease() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("foo", 1, 40));
        itemHolder.update();
        assertEquals(0, itemHolder.item.sellIn);
        assertEquals(39, itemHolder.item.quality);
    }

    @Test
    void fast_quality_decrease() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("foo", 0, 40));
        itemHolder.update();
        assertEquals(-1, itemHolder.item.sellIn);
        assertEquals(38, itemHolder.item.quality);
    }

    @Test
    void never_negative_quality() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("foo", -1, 1));
        itemHolder.update();
        assertEquals(-2, itemHolder.item.sellIn);
        assertEquals(0, itemHolder.item.quality);
    }

    @Test
    void aged_brie_quality_increase() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("Aged Brie", 5, 20));
        itemHolder.update();
        assertEquals(4, itemHolder.item.sellIn);
        assertEquals(21, itemHolder.item.quality);
    }

    @Test
    void aged_brie_max_quality_increase() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("Aged Brie", 5, 50));
        itemHolder.update();
        assertEquals(4, itemHolder.item.sellIn);
        assertEquals(50, itemHolder.item.quality);
    }

    @Test
    void sulfuras() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("Sulfuras, Hand of Ragnaros", 5, 80));
        itemHolder.update();
        assertEquals(5, itemHolder.item.sellIn);
        assertEquals(80, itemHolder.item.quality);
    }

    @Test
    void sulfuras_expired() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("Sulfuras, Hand of Ragnaros", -2, 80));
        itemHolder.update();
        assertEquals(-2, itemHolder.item.sellIn);
        assertEquals(80, itemHolder.item.quality);
    }

    @Test
    void backstage_passes_quality_increase() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20));
        itemHolder.update();
        assertEquals(14, itemHolder.item.sellIn);
        assertEquals(21, itemHolder.item.quality);
    }

    @Test
    void backstage_passes_max_quality() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("Backstage passes to a TAFKAL80ETC concert", 2, 50));
        itemHolder.update();
        assertEquals(1, itemHolder.item.sellIn);
        assertEquals(50, itemHolder.item.quality);
    }

    @Test
    void backstage_passes_day_ten() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("Backstage passes to a TAFKAL80ETC concert", 10, 20));
        itemHolder.update();
        assertEquals(9, itemHolder.item.sellIn);
        assertEquals(22, itemHolder.item.quality);
    }

    @Test
    void backstage_passes_day_five() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("Backstage passes to a TAFKAL80ETC concert", 5, 20));
        itemHolder.update();
        assertEquals(4, itemHolder.item.sellIn);
        assertEquals(23, itemHolder.item.quality);
    }

    @Test
    void backstage_passes_day_zero() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("Backstage passes to a TAFKAL80ETC concert", 1, 20));
        itemHolder.update();
        assertEquals(0, itemHolder.item.sellIn);
        assertEquals(23, itemHolder.item.quality);
    }

    @Test
    void backstage_passes_expired() {
        ItemHolder itemHolder = ItemHolderFactory.createItemHolder(new Item("Backstage passes to a TAFKAL80ETC concert", 0, 20));
        itemHolder.update();
        assertEquals(-1, itemHolder.item.sellIn);
        assertEquals(0, itemHolder.item.quality);
    }
}
