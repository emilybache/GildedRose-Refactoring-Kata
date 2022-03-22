package com.gildedrose;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeAll;

import static org.junit.jupiter.api.Assertions.*;


class GildedRoseTest {

    GildedRose app;

    @BeforeAll
    public void init() {
        Item[] items = new Item[] { new Item(ProjectConstants.SULFURAS, 0, 0) };
        app = new GildedRose(items);
    }

    @Test
    public void increaseByOne() {
        int initial = 2;
        app.increaseByOne(initial);
        assertEquals("3", initial);
    }

    @Test
    public void decreaseByOne() {
        int initial = 2;
        app.decreaseByOne(initial);
        assertEquals("1", initial);
    }

    @Test
    public void itemIsSulfuras() {
        String itemName = app.items[0].name;
        boolean result = app.itemIsSulfuras(itemName);
        assertTrue(result);
    }

    @Test
    public void ItemIsAgedBrieFalse() {
        String itemName = app.items[0].name;
        boolean result = app.itemIsAgedBrie(itemName);
        assertFalse(result);
    }
}
