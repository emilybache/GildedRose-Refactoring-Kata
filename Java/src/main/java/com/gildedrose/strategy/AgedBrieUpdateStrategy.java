package com.gildedrose.strategy;

import com.gildedrose.Item;

public class AgedBrieUpdateStrategy implements ItemUpdateStrategy {
    @Override
    public void update(Item item) {
        if (item.quality < 50) {
            item.quality += 1;
        }

        item.sellIn -= 1;

        if (item.sellIn < 0) {
            if (item.quality < 50) {
                item.quality += 1;
            }
        }
    }
}
