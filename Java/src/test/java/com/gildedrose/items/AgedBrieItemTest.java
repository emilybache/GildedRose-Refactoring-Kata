package com.gildedrose.items;

import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.item_helpers.ItemName.AGED_BRIE;
import static com.gildedrose.items.TestHelper.testItem;
import static com.gildedrose.items.TestHelper.testItemException;

@TestMethodOrder(OrderAnnotation.class)
class AgedBrieItemTest {

    private static final Item AGED_ITEM_INCREMENT_BY_ONE = new Item(AGED_BRIE.toString(), 5, 20);
    private static final Item AGED_ITEM_INCREMENT_QUALITY_BY_TWO = new Item(AGED_BRIE.toString(), 2, 4);
    private static final Item AGED_ITEM_ERROR = new Item(AGED_BRIE.toString(), 10, -5);

    @Test
    @Order(1)
    void incrementQualityByOneSuccess() {
        testItem(AGED_ITEM_INCREMENT_BY_ONE, 2, 22, 3);
    }

    @Test
    @Order(2)
    void incrementQualityByTwoSuccess() {
        testItem(AGED_ITEM_INCREMENT_QUALITY_BY_TWO, 3, 8, -1);
    }

    @Test
    @Order(3)
    void negativeQualityFail() {
        testItemException(AGED_ITEM_ERROR);
    }

}
