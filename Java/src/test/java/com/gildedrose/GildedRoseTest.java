package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    //GENERAL RULES
    @Test
    void whenSellInDateIsPassed_qualityDegradesTwiceAsFast() {
        //given
        Item testItem = new Item("standard item", 0, 3);
        Item[] items = new Item[] {testItem};

        //when
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        //then
        assertEquals(-1, app.items[0].sellIn);
        assertEquals(1, app.items[0].quality);
    }

    @Test
    void qualityOfAnItemIsNeverNegative() {
        //given
        Item testItem = new Item("standard item", 0, 0);
        Item[] items = new Item[] {testItem};

        //when
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        //then
        assertEquals(-1, app.items[0].sellIn);
        assertEquals(0, app.items[0].quality);
    }

    @Test
    void qualityOfAnItemIsNeverMoreThan50() {
        //given
        Item testItem = new Item("standard item", 5, 50);
        Item[] items = new Item[] {testItem};

        //when
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        //then
        assertEquals(4, app.items[0].sellIn);
        assertEquals(50, app.items[0].quality);
    }

    @Test
    void standardItemQualityDecreasesByOne() {
        //given
        Item testItem = new Item("standard item", 5, 2);
        Item[] items = new Item[] {testItem};

        //when
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        //then
        assertEquals(4, app.items[0].sellIn);
        assertEquals(1, app.items[0].quality);
    }

}
