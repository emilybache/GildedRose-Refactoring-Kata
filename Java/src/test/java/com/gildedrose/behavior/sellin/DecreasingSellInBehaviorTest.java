package com.gildedrose.behavior.sellin;

import com.gildedrose.Item;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class DecreasingSellInBehaviorTest {

    private SellInBehavior sellInBehavior;

    @BeforeEach
    public void setUp() throws Exception {
        sellInBehavior = new DecreasingSellInBehavior();
    }

    @Test
    void decreaseSellIn() {
        Item item = getItem(10);
        sellInBehavior.processSellInUpdate(item);

        assertEquals(9, item.sellIn);
    }

    @Test
    void decreaseSellInZero() {
        Item item = getItem(0);
        sellInBehavior.processSellInUpdate(item);

        assertEquals(-1, item.sellIn);
    }

    @Test
    void decreaseNegativeSellIn() {
        Item item = getItem(-1);
        sellInBehavior.processSellInUpdate(item);

        assertEquals(-2, item.sellIn);
    }

    private Item getItem(int sellIn) {
        return new Item("SomeItem", sellIn, 0);
    }
}
