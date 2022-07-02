package com.gildedrose;

import org.junit.jupiter.api.Test;

import static com.gildedrose.ItemTestHelper.assertItemEquals;

public class AgedBrieTest {

    @Test
    public void item_AgedBrie_increasesInQuality() {
        GildedRose app = new GildedRose(new Item("Aged Brie", 2, 2));

        app.updateQuality();

        assertItemEquals(app.getItems()[0], new Item("Aged Brie", 1, 3));
    }

    @Test
    public void item_AgedBrie_increasesInQuality_DoublesWhenOff() {
        GildedRose app = new GildedRose(new Item("Aged Brie", 0, 2));

        app.updateQuality();

        assertItemEquals(app.getItems()[0], new Item("Aged Brie", -1, 4));
    }

    @Test
    public void item_AgedBrie_cannotGoOver50Quality() {
        GildedRose app = new GildedRose(new Item("Aged Brie", 2, 50));

        app.updateQuality();

        assertItemEquals(app.getItems()[0], new Item("Aged Brie", 1, 50));
    }
}
