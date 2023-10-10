package com.gildedrose.strategy;

import com.gildedrose.Item;

public class BackstagePassesUpdateStrategy extends AbstractItemUpdateStrategy {
    @Override
    public void update(Item item) {
        incrementQualityIfLessThanMax(item);

        if (item.sellIn < 11) {
            incrementQualityIfLessThanMax(item);
        }

        if (item.sellIn < 6) {
            incrementQualityIfLessThanMax(item);
        }

        decrementSellIn(item);

        if (item.sellIn < 0) {
            item.quality = MIN_QUALITY;
        }
    }
}
