package com.gildedrose;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class BackstageItemTest {

    @Test
    void backstageIncreasesByOneOver10Days() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(21, item.quality);
    }

    @Test
    void backstageIncreasesByTwoBetween6And10Days() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(22, item.quality);
    }

    @Test
    void backstageIncreasesByThreeBetween1And5Days() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(23, item.quality);
    }

    @Test
    void backstageQualityDropsToZeroAfterConcert() {
        Item item = new Item("Backstage passes to a TAFKAL80ETC concert", 0, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(0, item.quality);
    }
}
