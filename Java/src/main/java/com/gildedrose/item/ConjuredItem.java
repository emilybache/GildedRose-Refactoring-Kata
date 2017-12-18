package com.gildedrose.item;

public class ConjuredItem extends StandardItem {

    public ConjuredItem(Item item) {
        super(item);
    }

    @Override
    public int decreasingValueOverZeroDaysToSell() {
        return 2;
    }
}
