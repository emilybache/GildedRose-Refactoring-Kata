package com.gildedrose.strategy;

import com.gildedrose.Item;

public class AgedBrieUpdateStrategy extends AbstractItemUpdateStrategy {
    @Override
    public void update(Item item) {
        incrementQualityIfLessThanMax(item);
        decrementSellIn(item);

        if (item.sellIn < 0) {
            incrementQualityIfLessThanMax(item);
        }
    }
}
