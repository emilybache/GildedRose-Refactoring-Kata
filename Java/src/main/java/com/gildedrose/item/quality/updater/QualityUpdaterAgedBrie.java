package com.gildedrose.item.quality.updater;

import com.gildedrose.Item;
import com.gildedrose.item.quality.QualityUpdater;

public class QualityUpdaterAgedBrie implements QualityUpdater {
    public void updateQuality(final Item item) {
        if (item.quality < MAX_LIMIT_QUALITY) {
            increaseQuality(item);
        }
        item.sellIn--;
        if (item.sellIn < MIM_LIMIT_SELL_IN) {
            increaseQuality(item);
        }
    }

    private void increaseQuality(final Item item) {
        if (item.quality < MAX_LIMIT_QUALITY) {
            item.quality++;
        }
    }
}
