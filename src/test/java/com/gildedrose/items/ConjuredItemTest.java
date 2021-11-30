package com.gildedrose.items;

import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.helper.TestHelper.testItem;
import static com.gildedrose.helper.TestHelper.testItemException;
import static com.gildedrose.item_helpers.ItemName.CONJURED;

@TestMethodOrder(OrderAnnotation.class)
class ConjuredItemTest {

    private final Item item = new Item(CONJURED.toString(), 5, 20);
    private final Item itemError = new Item(CONJURED.toString(), 10, -5);

    @Test
    @Order(1)
    void decrementQualityByTwoSuccess() {
        testItem(item, 2, 3, 16);
    }

    @Test
    @Order(2)
    void decrementQualityByFourSuccess() {
        testItem(item, 10, -5, 0);
    }

    @Test
    @Order(3)
    void negativeQualityFail() {
        testItemException(itemError);
    }
}
