package com.gildedrose.strategy;

import com.gildedrose.Item;

public class SulfurasItemStrategyImpl implements ItemStrategy {
    @Override
    public void updateQuality(Item item) {
        // Quality remains unchanged. Not for sale!
    }
}
