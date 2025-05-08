package com.gildedrose.updaters;

import com.gildedrose.Item;

public abstract class ItemUpdater {
    protected final Item item;

    public ItemUpdater(Item item) {
        this.item = item;
    }

    public abstract void update();

    protected void increaseQuality(int amount) {
        item.quality = Math.min(50, item.quality + amount);
    }

    protected void decreaseQuality(int amount) {
        item.quality = Math.max(0, item.quality - amount);
    }

    protected void decreaseSellIn() {
        item.sellIn--;
    }
}
