package com.gildedrose;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.BeforeAll;

import static org.junit.jupiter.api.Assertions.*;

@TestInstance(Lifecycle.PER_CLASS)
public class GildedRoseTest {

    GildedRose app;

    Item[] items;

    @BeforeAll
    public void init() {
        items = new Item[] { new Item(ProjectConstants.SULFURAS, 0, 0),
                                new Item(ProjectConstants.ELIXIR, 2, 54) };
        app = new GildedRose(items);
    }

    @Test
    public void increaseByOneTest() {
        int result = app.increaseByOne(2);
        assertEquals(3, result);
    }

    @Test
    public void decreaseByOneTest() {
        int result = app.decreaseByOne(2);
        assertEquals(1, result);
    }

    @Test
    public void itemIsSulfurasTest() {
        String itemName = app.items[0].name;
        boolean result = app.itemIsSulfuras(itemName);
        assertTrue(result);
    }

    @Test
    public void ItemIsAgedBrieFalseTest() {
        String itemName = app.items[0].name;
        boolean result = app.itemIsAgedBrie(itemName);
        assertFalse(result);
    }

    @Test
    public void qualityComparisonTest() {
        int firstItemQuality = app.items[0].quality;
        int secondItemQuality = app.items[1].quality;

        assertFalse(app.qualityHigherThanZero(firstItemQuality));
        assertTrue(app.qualityHigherThanZero(secondItemQuality));

        assertTrue(app.qualityLowerThanFifty(firstItemQuality));
        assertFalse(app.qualityLowerThanFifty(secondItemQuality));
    }

    @Test
    public void isDecreaseItemTest() {
        String firstItemName = app.items[0].name;
        int firstItemQuality = app.items[0].quality;
        String secondItemName = app.items[1].name;
        int secondItemQuality = app.items[1].quality;

        assertFalse(app.isDecreasableItem(firstItemName, firstItemQuality));
        assertTrue(app.isDecreasableItem(secondItemName, secondItemQuality));
    }
}
