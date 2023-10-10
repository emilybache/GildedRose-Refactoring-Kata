package com.gildedrose.strategy;

import com.gildedrose.Item;

public class StandardItemUpdateStrategy extends AbstractItemUpdateStrategy {
    @Override
    public void update(Item item) {
        decrementSellIn(item);
        if (item.quality > MIN_QUALITY) {
            item.quality -= 1;
        }
        if (item.sellIn < 0 && item.quality > MIN_QUALITY) {
            item.quality -= 1;
        }
    }
}
