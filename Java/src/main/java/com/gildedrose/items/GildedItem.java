package com.gildedrose.items;

import com.gildedrose.Item;

public abstract class GildedItem {

    protected Item item;

    public GildedItem(Item item) {
        this.item = item;
    }

    public abstract void updateQuality();

    protected void decreaseQuality(int amount) {
        item.quality = Math.max(0, item.quality - amount);
    }

    protected void increaseQuality(int amount) {
        item.quality = Math.min(50, item.quality + amount);
    }

    protected void decreaseSellIn() {
        item.sellIn--;
    }
}
