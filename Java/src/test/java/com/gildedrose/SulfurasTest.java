package com.gildedrose;

import org.junit.jupiter.api.Test;

import static com.gildedrose.ItemTestHelper.assertItemEquals;

public class SulfurasTest {

    @Test
    public void item_Sulfuras_neverChanges() {
        GildedRose app = new GildedRose(new Item("Sulfuras, Hand of Ragnaros", 100, 100));

        app.updateQuality();

        assertItemEquals(app.getItems()[0], new Item("Sulfuras, Hand of Ragnaros", 100, 100));
    }
}
