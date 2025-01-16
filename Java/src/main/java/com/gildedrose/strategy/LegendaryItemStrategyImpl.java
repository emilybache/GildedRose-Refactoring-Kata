package com.gildedrose.strategy;

import com.gildedrose.Item;

public class LegendaryItemStrategyImpl implements ItemStrategy {
    @Override
    public void updateQuality(Item item) {
        // Quality remains unchanged. Not for sale!
    }
}
