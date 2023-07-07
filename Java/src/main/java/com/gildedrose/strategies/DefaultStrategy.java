package com.gildedrose.strategies;

import com.gildedrose.Item;

public enum DefaultStrategy implements QualityUpdateStrategy {
    INSTANCE;

    @Override
    public void applyTo(Item item) {
        item.quality = Math.max(0, item.sellIn > 0 ? (item.quality-1) : (item.quality-2));
        --item.sellIn;
    }
}
