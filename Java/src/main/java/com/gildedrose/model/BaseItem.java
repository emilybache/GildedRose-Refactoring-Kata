package com.gildedrose.model;

import com.gildedrose.Item;

public class BaseItem {
    private final Item item;
    public BaseItem(Item item) {
        this.item = item;
    }

    public int getSellIn() {
        return item.sellIn;
    }

    public void setSellIn(int sellIn) {
        this.item.sellIn = sellIn;
    }
}
