package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class  ItemTest {

    @Test
    void contructorWithThreeArguments() {
        Item[] items = new Item[]{new Item("Elixir of the Mongoose", 5, 7)};
        assertEquals("Elixir of the Mongoose", items[0].getName());
        assertEquals(7, items[0].getQuality());
        assertEquals(5, items[0].getSellIn());
    }

    @Test
    void getterName() {
        Item[] items = new Item[]{new Item("Elixir of the Mongoose", 0, 0)};
        assertEquals("Elixir of the Mongoose", items[0].getName());
    }

    @Test
    void getterSellin() {
        Item[] items = new Item[]{new Item("Elixir of the Mongoose", 7, 0)};
        assertEquals(7, items[0].getSellIn());
        assertEquals(0, items[0].getQuality());

    }

    @Test
    void setterSellin() {
        Item[] items = new Item[]{new Item("Elixir of the Mongoose", 7, 0)};
        items[0].setSellIn(1);
        assertEquals(1, items[0].getSellIn());
        assertEquals(0, items[0].getQuality());
    }


    @Test
    void getterQuality() {
        Item[] items = new Item[]{new Item("Elixir of the Mongoose", 0, 5)};
        assertEquals(5, items[0].getQuality());
        assertEquals(0, items[0].getSellIn());
    }

    @Test
    void setterQuality() {
        Item[] items = new Item[]{new Item("Elixir of the Mongoose", 0, 5)};
        items[0].setQuality(1);
        assertEquals(1, items[0].getQuality());
        assertEquals(0, items[0].getSellIn());
    }
}
