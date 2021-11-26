package com.gildedrose.items.items;

import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.item_helpers.ItemName.NORMAL;
import static com.gildedrose.items.helper.TestHelper.testItem;
import static com.gildedrose.items.helper.TestHelper.testItemException;

@TestMethodOrder(OrderAnnotation.class)
class NormalItemTest {

    private final Item item = new Item(NORMAL.toString(), 5, 20);
    private final Item itemError = new Item(NORMAL.toString(), 10, -5);

    @Test
    @Order(1)
    void decrementQualityByOneSuccess() {
        testItem(item, 2, 3, 18);
    }

    @Test
    @Order(2)
    void decrementQualityByTwoSuccess() {
        testItem(item, 10, -5, 5);
    }

    @Test
    @Order(3)
    void negativeQualityFail() {
        testItemException(itemError);
    }
}
