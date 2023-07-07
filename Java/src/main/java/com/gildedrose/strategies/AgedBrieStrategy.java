package com.gildedrose.strategies;

import com.gildedrose.Item;

public enum AgedBrieStrategy implements QualityUpdateStrategy {
    INSTANCE;

    @Override
    public void applyTo(Item item) {
        item.quality = Math.min(50, item.sellIn <= 0 ? item.quality+2 : item.quality+1);
        --item.sellIn;
    }
}
