package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }

    @Test
    void testconjuredTicket() {
        Item[] items = new Item[] { new Item("Conjured Mana Cake", 3, 6) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(2, app.items[0].sellIn);
        assertEquals(4, app.items[0].quality);
    }

    @Test
    void testconjuredTicketNeverNegative() {
        Item[] items = new Item[] { new Item("Conjured Mana Cake", 3, 1) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(2, app.items[0].sellIn);
        assertEquals(0, app.items[0].quality);
    }

}
