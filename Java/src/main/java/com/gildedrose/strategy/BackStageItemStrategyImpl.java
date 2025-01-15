package com.gildedrose.strategy;

import com.gildedrose.Item;

public class BackStageItemStrategyImpl implements ItemStrategy {
    @Override
    public void updateQuality(Item item) {
        item.sellIn--;

        if (item.sellIn < 0) {
            item.quality = 0;
            return;
        }

        int increment = INCREASE_RATE;
        if (item.sellIn < 5) {
            increment += 2;
        } else if (item.sellIn < 10) {
            increment++;
        }

        increaseQuality(item, increment);
    }
}
