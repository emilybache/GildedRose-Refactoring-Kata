package com.gildedrose.items;

import com.gildedrose.main.GildedRose;
import com.gildedrose.main.Item;

import static com.gildedrose.item_helpers.ItemHandler.QUALITY_ERROR_MESSAGE;
import static org.junit.jupiter.api.Assertions.*;

public class TestHelper {

    public static void testItem(Item item, int days, int expectedQuality, int expectedSellIn) {
        // given
        GildedRose app = new GildedRose(item);
        // when
        for (int i = 0; i < days; i++) {
            app.updateQuality();
        }
        //then
        assertEquals(expectedQuality, item.quality);
        assertEquals(expectedSellIn, item.sellIn);
    }

    public static void testItemException(Item item) {
        GildedRose gildedRose = new GildedRose(item);
        Exception exception = assertThrows(IllegalArgumentException.class, gildedRose::updateQuality);
        String actualMessage = exception.getMessage();
        assertTrue(actualMessage.contains(QUALITY_ERROR_MESSAGE));
    }
}
