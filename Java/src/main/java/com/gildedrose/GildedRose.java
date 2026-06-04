package com.gildedrose;

import com.gildedrose.strategy.ItemStrategyFactory;
import com.gildedrose.strategy.ItemStrategyUpdate;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            ItemStrategyUpdate strategy = ItemStrategyFactory.getStrategy(items[i]);
            strategy.update(items[i]);
        }
    }
}

