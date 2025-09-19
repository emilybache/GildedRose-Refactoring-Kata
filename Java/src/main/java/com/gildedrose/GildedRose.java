package com.gildedrose;

import com.gildedrose.strategy.StrategyProvider;
import com.gildedrose.strategy.UpdateStrategy;

class GildedRose {
    Item[] items;
    private final StrategyProvider strategyProvider;

    public GildedRose(Item[] items) {
        this.items = items;
        this.strategyProvider = new StrategyProvider();
    }

    public GildedRose(Item[] items, StrategyProvider strategyProvider) {
        this.items = items;
        this.strategyProvider = strategyProvider;
    }

    public void updateQuality() {
        for (Item item : items) {
            UpdateStrategy strategy = this.strategyProvider.getStrategyForItem(item);
            strategy.update(item);
        }
    }
}
