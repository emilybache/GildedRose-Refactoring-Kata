package com.gildedrose.experts;

import com.gildedrose.Item;
import com.gildedrose.ItemUpdater;

public class AgedBrieUpdater implements ItemUpdater {
    @Override
    public void update(Item item) {
        item.sellIn--;
        if (item.quality < 50) {
            item.quality++;
        }
        if (item.sellIn < 0 && item.quality < 50) {
            item.quality++;
        }
    }
}
