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
        item.sellIn = sellIn;
    }

    public void update() {
        decrementSellIn();
    }

    private void decrementSellIn() {
        setSellIn(getSellIn() - 1);
    }
}
