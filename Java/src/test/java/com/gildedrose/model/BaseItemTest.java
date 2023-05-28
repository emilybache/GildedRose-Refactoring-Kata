package com.gildedrose.model;

import com.gildedrose.Item;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class BaseItemTest {

    @Test
    void itEncapsulatesTheSellInValueOfTheItem() {
        BaseItem baseItem = new BaseItem(new Item("foo", 10, 20));
        assertEquals(10, baseItem.getSellIn());
    }

    @Test
    void itMutatesTheSellInValueOfTheItem() {
        BaseItem baseItem = new BaseItem(new Item("foo", 10, 20));
        baseItem.setSellIn(9);
        assertEquals(9, baseItem.getSellIn());
    }
}
