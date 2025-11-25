package com.gildedrose;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class SulfurasItemTest {

    @Test
    void sulfurasNeverChanges() {
        Item item = new Item("Sulfuras, Hand of Ragnaros", 0, 80);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(0, item.sellIn);
        assertEquals(80, item.quality);
    }
}
