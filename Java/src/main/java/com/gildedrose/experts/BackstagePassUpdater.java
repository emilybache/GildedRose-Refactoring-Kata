package com.gildedrose.experts;

import com.gildedrose.Item;
import com.gildedrose.ItemUpdater;

public class BackstagePassUpdater implements ItemUpdater {
    @Override
    public void update(Item item) {
        if (item.quality < 50) {
            item.quality++;
            if (item.sellIn < 11 && item.quality < 50) {
                item.quality++;

            }
            if (item.sellIn < 6 && item.quality < 50) {
                item.quality++;
            }
        }
        item.sellIn -= 1;

        if (item.sellIn < 0) {
            item.quality = 0;
        }
    }
}
