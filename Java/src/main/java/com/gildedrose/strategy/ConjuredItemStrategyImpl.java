package com.gildedrose.strategy;

import com.gildedrose.Item;

public class ConjuredItemStrategyImpl implements ItemStrategy {
    @Override
    public void updateQuality(Item item) {
        item.sellIn--;
        int decrement = item.sellIn < 0 ? 4 * DEGRADATION_RATE : 2 * DEGRADATION_RATE;
        decreaseQuality(item, decrement);
    }
}
