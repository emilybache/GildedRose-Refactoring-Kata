package com.gildedrose;

import com.gildedrose.main.GildedRose;
import com.gildedrose.main.Item;
import org.junit.jupiter.api.Test;

import static com.gildedrose.item_helpers.ItemName.NORMAL;
import static org.junit.jupiter.api.Assertions.*;

class NormalItemTest {

    @Test
    void decrementQualityByOneSuccess() {
        // given
        Item normalItem = new Item(NORMAL.toString(), 5, 20);
        GildedRose app = new GildedRose(normalItem);
        // when
        int days = 2;
        for (int i = 0; i < days; i++) {
            app.updateQuality();
        }
        //then
        assertEquals(18, normalItem.quality);
        assertEquals(3, normalItem.sellIn);
    }

    @Test
    void decrementQualityByTwoSuccess() {
        // given
        Item normalItem = new Item(NORMAL.toString(), 10, 20);
        GildedRose app = new GildedRose(normalItem);
        // when
        int days = 15;
        for (int i = 0; i < days; i++) {
            app.updateQuality();
        }
        //then
        assertEquals(0, normalItem.quality);
        assertEquals(-5, normalItem.sellIn);
    }

    @Test
    void negativeQualityFail() {
        Item normalItem = new Item(NORMAL.toString(), 10, -5);
        GildedRose app = new GildedRose(normalItem);
        Exception exception = assertThrows(IllegalArgumentException.class, app::updateQuality);
        String actualMessage = exception.getMessage();
        assertTrue(actualMessage.contains("Quality cannot be negative! Current value:"));
    }
}
