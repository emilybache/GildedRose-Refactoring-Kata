package com.gildedrose.strategy;

import com.gildedrose.Item;

public class AgedBrieItemStrategyImpl implements ItemStrategy {
    @Override
    public void updateQuality(Item item) {
        item.sellIn--;
        int increment = item.sellIn < 0 ? 2 * INCREASE_RATE : INCREASE_RATE;
        increaseQuality(item, increment);
    }
}
