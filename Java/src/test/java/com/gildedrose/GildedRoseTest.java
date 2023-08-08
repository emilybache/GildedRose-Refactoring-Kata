package com.gildedrose;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class GildedRoseTest {

    private Item[] items;

    @BeforeEach
    public void setUp() {
        items = new Item[]{
            new Item("+5 Dexterity Vest", 10, 20),
            new Item("Aged Brie", 2, 0),
            new Item("Elixir of the Mongoose", 5, 7),
            new Item("Sulfuras, Hand of Ragnaros", 0, 80),
            new Item("Sulfuras, Hand of Ragnaros", -1, 80),
            new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
            new Item("Conjured Mana Cake", 3, 6)
        };
    }

    @Test
    public void testDay1() {
        GildedRose.updateQuality(items);

        // Assertions for Day 1
        assertEquals(9, items[0].sellIn);
        assertEquals(19, items[0].quality);

        assertEquals(1, items[1].sellIn);
        assertEquals(1, items[1].quality);

        assertEquals(4, items[2].sellIn);
        assertEquals(6, items[2].quality);

        assertEquals(0, items[3].sellIn);
        assertEquals(80, items[3].quality);

        assertEquals(-1, items[4].sellIn);
        assertEquals(80, items[4].quality);

        assertEquals(14, items[5].sellIn);
        assertEquals(21, items[5].quality);

        assertEquals(9, items[6].sellIn);
        assertEquals(50, items[6].quality);

        assertEquals(4, items[7].sellIn);
        assertEquals(50, items[7].quality);

        assertEquals(2, items[8].sellIn);
        assertEquals(4, items[8].quality);
    }

    @Test
    public void testDay2() {
        GildedRose.updateQuality(items);
        GildedRose.updateQuality(items);

        // Assertions for Day 2
        assertEquals(8, items[0].sellIn);
        assertEquals(18, items[0].quality);

        assertEquals(0, items[1].sellIn);
        assertEquals(2, items[1].quality);

        assertEquals(3, items[2].sellIn);
        assertEquals(5, items[2].quality);

        assertEquals(0, items[3].sellIn);
        assertEquals(80, items[3].quality);

        assertEquals(-1, items[4].sellIn);
        assertEquals(80, items[4].quality);

        assertEquals(13, items[5].sellIn);
        assertEquals(22, items[5].quality);

        assertEquals(8, items[6].sellIn);
        assertEquals(50, items[6].quality);

        assertEquals(3, items[7].sellIn);
        assertEquals(50, items[7].quality);

        assertEquals(1, items[8].sellIn);
        assertEquals(2, items[8].quality);
    }

    @Test
    public void testDay100() {
        GildedRose app = new GildedRose();
        for (int i = 0; i < 100; i++) {
            app.updateQuality(items);
        }

        // Assertions for Day 100
        assertEquals(-90, items[0].sellIn);
        assertEquals(0, items[0].quality);

        assertEquals(-98, items[1].sellIn);
        assertEquals(50, items[1].quality);

        assertEquals(-95, items[2].sellIn);
        assertEquals(0, items[2].quality);

        assertEquals(0, items[3].sellIn);
        assertEquals(80, items[3].quality);

        assertEquals(-1, items[4].sellIn);
        assertEquals(80, items[4].quality);

        assertEquals(-85, items[5].sellIn);
        assertEquals(0, items[5].quality);

        assertEquals(-90, items[6].sellIn);
        assertEquals(0, items[6].quality);

        assertEquals(-95, items[7].sellIn);
        assertEquals(0, items[7].quality);

        assertEquals(-97, items[8].sellIn);
        assertEquals(0, items[8].quality);
    }
}
