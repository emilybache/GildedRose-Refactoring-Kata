package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[]{
                new Item("+5 Dexterity Vest", 10, 20), //
                new Item("Aged Brie", 2, 0), //
                new Item("Elixir of the Mongoose", 5, 7), //
                new Item("Sulfuras, Hand of Ragnaros", 0, 80), //
                new Item("Sulfuras, Hand of Ragnaros", -1, 80),
                new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
                new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
                new Item("Backstage passes to a TAFKAL80ETC concert", 5, 47),
                new Item("Backstage passes to a TAFKAL80ETC concert", 0, 47),
                new Item("Conjured Mana Cake", 3, 6) };

        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertEquals("+5 Dexterity Vest", app.items[0].name);
        assertEquals(9, app.items[0].sellIn);
        assertEquals(19, app.items[0].quality);
        assertEquals("Aged Brie", app.items[1].name);
        assertEquals(1, app.items[1].sellIn);
        assertEquals(1, app.items[1].quality);
        assertEquals("Elixir of the Mongoose", app.items[2].name);
        assertEquals(4, app.items[2].sellIn);
        assertEquals(6, app.items[2].quality);
        assertEquals("Sulfuras, Hand of Ragnaros", app.items[3].name);
        assertEquals(0, app.items[3].sellIn);
        assertEquals(80, app.items[3].quality);
        assertEquals("Sulfuras, Hand of Ragnaros", app.items[4].name);
        assertEquals(-1, app.items[4].sellIn);
        assertEquals(80, app.items[4].quality);
        assertEquals("Backstage passes to a TAFKAL80ETC concert", app.items[5].name);
        assertEquals(14, app.items[5].sellIn);
        assertEquals(21, app.items[5].quality);
        assertEquals("Backstage passes to a TAFKAL80ETC concert", app.items[6].name);
        assertEquals(9, app.items[6].sellIn);
        assertEquals(50, app.items[6].quality);
        assertEquals("Backstage passes to a TAFKAL80ETC concert", app.items[7].name);
        assertEquals(4, app.items[7].sellIn);
        assertEquals(50, app.items[7].quality);
        assertEquals("Backstage passes to a TAFKAL80ETC concert", app.items[8].name);
        assertEquals(-1, app.items[8].sellIn);
        assertEquals(0, app.items[8].quality);

        assertEquals("Conjured Mana Cake", app.items[9].name);
        assertEquals(2, app.items[9].sellIn);
        assertEquals(4, app.items[9].quality);
    }

}
