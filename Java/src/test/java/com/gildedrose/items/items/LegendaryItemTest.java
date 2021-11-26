package com.gildedrose.items.items;

import com.gildedrose.main.GildedRose;
import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.item_helpers.ItemName.LEGENDARY;
import static com.gildedrose.items.LegendaryItem.LEGENDARY_ITEM_QUALITY;
import static com.gildedrose.items.LegendaryItem.NOT_LEGENDARY_ITEM_ERROR_MESSAGE;
import static com.gildedrose.items.helper.TestHelper.testItem;
import static com.gildedrose.items.helper.TestHelper.testItemException;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

@TestMethodOrder(OrderAnnotation.class)
class LegendaryItemTest {

    private final Item item = new Item(LEGENDARY.toString(), 5, 80);
    private final Item fakeLegendaryItem = new Item(LEGENDARY.toString(), 5, 75);
    private final Item itemError = new Item(LEGENDARY.toString(), 10, -5);

    @Test
    @Order(1)
    void getLegendaryQualityBeforeSellInDateSuccess() {
        testItem(item, 2, 3, LEGENDARY_ITEM_QUALITY);
    }

    @Test
    @Order(2)
    void getLegendaryQualityPassSellInDateSuccess() {
        testItem(item, 10, -5, LEGENDARY_ITEM_QUALITY);
    }

    @Test
    @Order(3)
    void testFakeLegendaryItemExceptionFail() {
        GildedRose gildedRose = new GildedRose(fakeLegendaryItem);
        Exception exception = assertThrows(IllegalArgumentException.class, gildedRose::updateQuality);
        String actualMessage = exception.getMessage();
        assertTrue(actualMessage.contains(NOT_LEGENDARY_ITEM_ERROR_MESSAGE));
    }


    @Test
    @Order(4)
    void negativeQualityFail() {
        testItemException(itemError);
    }
}
