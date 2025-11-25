package com.gildedrose;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class NormalItemTest {

    @Test
    void normalItemDegradesByOneBeforeSellIn() {
        Item item = new Item("Normal Item", 5, 10);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(4, item.sellIn);
        assertEquals(9, item.quality);
    }

    @Test
    void normalItemDegradesTwiceAfterSellIn() {
        Item item = new Item("Normal Item", 0, 10);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(-1, item.sellIn);
        assertEquals(8, item.quality);
    }

    @Test
    void qualityNeverNegative() {
        Item item = new Item("Normal Item", 0, 0);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(0, item.quality);
    }
}
