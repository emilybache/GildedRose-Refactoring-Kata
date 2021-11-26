package com.gildedrose.items;

import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.item_helpers.ItemName.BACKSTAGE_PASS;
import static com.gildedrose.items.TestHelper.testItem;
import static com.gildedrose.items.TestHelper.testItemException;

@TestMethodOrder(OrderAnnotation.class)
class BackstagePassItemTest {

    private final Item item = new Item(BACKSTAGE_PASS.toString(), 15, 20);
    private final Item itemError = new Item(BACKSTAGE_PASS.toString(), 10, -5);

    @Test
    @Order(1)
    void testIncrementQualityByOneSuccess() {
        testItem(item, 5, 10, 25);
    }

    @Test
    @Order(2)
    void testIncrementQualityByTwoSuccess() {
        testItem(item, 10, 5, 35);
    }

    @Test
    @Order(3)
    void testIncrementQualityByThreeSuccess() {
        testItem(item, 15, 0, 50);
    }

    @Test
    @Order(4)
    void testQualityZeroSuccess() {
        testItem(item, 16, -1, 0);
    }

    @Test
    @Order(2)
    void negativeQualityFail() {
        testItemException(itemError);
    }

}
