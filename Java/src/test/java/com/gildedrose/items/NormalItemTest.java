package com.gildedrose.items;

import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.item_helpers.ItemName.NORMAL;
import static com.gildedrose.items.TestHelper.testItem;
import static com.gildedrose.items.TestHelper.testItemException;

@TestMethodOrder(OrderAnnotation.class)
class NormalItemTest {

    private static final Item NORMAL_ITEM = new Item(NORMAL.toString(), 5, 20);
    private static final Item NORMAL_ITEM_INCREMENT_BY_TWO = new Item(NORMAL.toString(), 10, 20);
    private static final Item NORMAL_ITEM_ERROR = new Item(NORMAL.toString(), 10, -5);

    @Test
    @Order(1)
    void decrementQualityByOneSuccess() {
        testItem(NORMAL_ITEM, 2, 18, 3);
    }

    @Test
    @Order(2)
    void decrementQualityByTwoSuccess() {
        testItem(NORMAL_ITEM_INCREMENT_BY_TWO, 15, 0, -5);
    }

    @Test
    @Order(3)
    void negativeQualityFail() {
        testItemException(NORMAL_ITEM_ERROR);
    }
}
