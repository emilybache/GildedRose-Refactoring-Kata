package com.gildedrose.behavior.sellin;

import com.gildedrose.Item;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ImmutableSellInBehaviorTest {

    private SellInBehavior sellInBehavior;

    @BeforeEach
    public void setUp() throws Exception {
        sellInBehavior = new ImmutableSellInBehavior();
    }

    @Test
    void immutableSellIn() {
        Item item = getItem(10);
        sellInBehavior.processSellInUpdate(item);

        assertEquals(10, item.sellIn);
    }

    @Test
    void immutableSellInZero() {
        Item item = getItem(0);
        sellInBehavior.processSellInUpdate(item);

        assertEquals(0, item.sellIn);
    }

    @Test
    void immutableNegativeSellIn() {
        Item item = getItem(-1);
        sellInBehavior.processSellInUpdate(item);

        assertEquals(-1, item.sellIn);
    }

    private Item getItem(int sellIn) {
        return new Item("SomeItem", sellIn, 0);
    }
}
