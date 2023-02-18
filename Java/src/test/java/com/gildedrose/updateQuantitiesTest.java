package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;


/**
 * Testing for Selling items
 */

class updateQuantitiesTest {
    @Test
    void itemsSoldAfterConcert() {
        Item[] items = new Item[] {
            new Item("+5 Dexterity Vest", 10, 20), //
            new Item("Aged Brie", 2, 0), //
            new Item("Elixir of the Mongoose", 5, 7), //
            new Item("Sulfuras, Hand of Ragnaros", 0, 80), //
            new Item("Sulfuras, Hand of Ragnaros", -1, 80),
            new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuantities();
//        assertEquals(, app.items.length);
    }

    @Test
    void itemBackstageQualityDropsToZeroAfterTheConcert() {
        System.out.println("Quality drops to 0 after the concert");
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 2),
            new Item("Backstage passes to a TAFKAL80ETC concert", 9, 3),
            new Item("Backstage passes to a TAFKAL80ETC concert", 8, 7)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuantities();
        for ( Item el: items) {
            assertEquals(0, el.quality);
        }
    }

    @Test
    void itemSulfurasNotSold() {
        System.out.println("\"Sulfuras\", being a legendary item, never has to be sold");
        Item[] items = new Item[] {
            new Item("+5 Dexterity Vest", 10, 20), //
            new Item("Aged Brie", 2, 0), //
            new Item("Elixir of the Mongoose", 5, 7), //
            new Item("Sulfuras, Hand of Ragnaros", 0, 80), //
            new Item("Sulfuras, Hand of Ragnaros", -1, 80),
            new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuantities();
        assertEquals(2, app.items.length);
    }

}


