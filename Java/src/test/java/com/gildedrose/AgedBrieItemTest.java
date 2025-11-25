package com.gildedrose;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class AgedBrieItemTest {

    @Test
    void agedBrieIncreasesQuality() {
        Item item = new Item("Aged Brie", 2, 0);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(1, item.quality);
        assertEquals(1, item.sellIn);
    }

    @Test
    void qualityNeverExceeds50() {
        Item item = new Item("Aged Brie", 2, 50);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(50, item.quality);
    }
}
