package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void givenListOfItems_whenUpdateQuality_thenNameRemainsUnchanged() {
        final var name = "foo";

        var items = new Item[] { new Item(name, 0, 0) };
        var app = new GildedRose(items);

        app.updateQuality();

        assertEquals(name, app.items[0].getName());
    }


}
