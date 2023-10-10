package com.gildedrose.strategy;

import com.gildedrose.Item;

public abstract class AbstractItemUpdateStrategy implements ItemUpdateStrategy {
    protected static final int MAX_QUALITY = 50;
    protected static final int MIN_QUALITY = 0;

    protected void incrementQualityIfLessThanMax(Item item) {
        if (item.quality < MAX_QUALITY) {
            item.quality += 1;
        }
    }

    protected void decrementSellIn(Item item) {
        item.sellIn -= 1;
    }
}
