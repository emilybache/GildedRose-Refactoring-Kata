package com.gildedrose;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {
    @Test
    @DisplayName("Regular item")
    void updateQualityRegularItem() {
        Item itemUnderTest = new Item("Sample Regular Item", 1, 5);
        Item[] items = new Item[] { itemUnderTest };
        GildedRose app = new GildedRose(items);

        app.updateQuality();
        assertEquals(0, itemUnderTest.sellIn);
        assertEquals(4, itemUnderTest.quality);

        app.updateQuality();
        assertEquals(-1, itemUnderTest.sellIn);
        assertEquals(2, itemUnderTest.quality);

        app.updateQuality();
        assertEquals(-2, itemUnderTest.sellIn);
        assertEquals(0, itemUnderTest.quality);

        app.updateQuality();
        assertEquals(-3, itemUnderTest.sellIn);
        assertEquals(0, itemUnderTest.quality);
    }

    @Test
    @DisplayName("Aged Brie")
    void updateQualityAgedBrie() {
        Item agedBrieUnderTest = new Item("Aged Brie", 1, 46);
        Item[] items = new Item[] { agedBrieUnderTest };
        GildedRose app = new GildedRose(items);

        app.updateQuality();
        assertEquals(0, agedBrieUnderTest.sellIn);
        assertEquals(47, agedBrieUnderTest.quality);

        app.updateQuality();
        assertEquals(-1, agedBrieUnderTest.sellIn);
        assertEquals(49, agedBrieUnderTest.quality);

        app.updateQuality();
        assertEquals(-2, agedBrieUnderTest.sellIn);
        assertEquals(50, agedBrieUnderTest.quality);

        app.updateQuality();
        assertEquals(-3, agedBrieUnderTest.sellIn);
        assertEquals(50, agedBrieUnderTest.quality);
    }

    @Test
    @DisplayName("Sulfuras")
    void updateQualitySulfuras() {
        Item sulfurasUnderTest = new Item("Sulfuras, Hand of Ragnaros", 23, 80);
        Item[] items = new Item[] { sulfurasUnderTest };
        GildedRose app = new GildedRose(items);

        app.updateQuality();
        assertEquals(23, sulfurasUnderTest.sellIn);
        assertEquals(80, sulfurasUnderTest.quality);

        app.updateQuality();
        assertEquals(23, sulfurasUnderTest.sellIn);
        assertEquals(80, sulfurasUnderTest.quality);

        app.updateQuality();
        assertEquals(23, sulfurasUnderTest.sellIn);
        assertEquals(80, sulfurasUnderTest.quality);
    }

    @Test
    @DisplayName("Backstage Pass")
    void updateQualityBackstagePasses() {
        Item bPass11days2event = new Item("Backstage passes to a TAFKAL80ETC concert", 11, 25);
        Item bPass10days2event = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 25);
        Item bPass5days2event = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 25);
        Item bPass1dayB4event = new Item("Backstage passes to a TAFKAL80ETC concert", 1, 47);
        Item[] items = new Item[] {
            bPass11days2event, bPass10days2event,
            bPass5days2event, bPass1dayB4event
        };
        GildedRose app = new GildedRose(items);

        app.updateQuality();
        assertEquals(26, bPass11days2event.quality); //now 10 days
        assertEquals(27, bPass10days2event.quality); //now 9 days
        assertEquals(28, bPass5days2event.quality); //now 4 days
        assertEquals(50, bPass1dayB4event.quality); //now event day

        app.updateQuality();
        assertEquals(28, bPass11days2event.quality); //now 9 days
        assertEquals(29, bPass10days2event.quality); //now 8 days
        assertEquals(31, bPass5days2event.quality); //now 3 days
        assertEquals(0, bPass1dayB4event.quality); //past event date

        app.updateQuality();
        assertEquals(30, bPass11days2event.quality); //now 8 days
        assertEquals(31, bPass10days2event.quality); //now 7 days
        assertEquals(34, bPass5days2event.quality); //now 2 days
        assertEquals(0, bPass1dayB4event.quality); //past event date
    }

    @Test
    @Disabled
    @DisplayName("Conjured Item")
    void updateQualityConjuredItem() {
        Item conjuredItemUnderTest = new Item("Conjured Sample Item", 1, 8);
        Item[] items = new Item[] { conjuredItemUnderTest };
        GildedRose app = new GildedRose(items);

        app.updateQuality();
        assertEquals(0, conjuredItemUnderTest.sellIn);
        assertEquals(6, conjuredItemUnderTest.quality);

        app.updateQuality();
        assertEquals(-1, conjuredItemUnderTest.sellIn);
        assertEquals(2, conjuredItemUnderTest.quality);

        app.updateQuality();
        assertEquals(-2, conjuredItemUnderTest.sellIn);
        assertEquals(0, conjuredItemUnderTest.quality);
    }
}
