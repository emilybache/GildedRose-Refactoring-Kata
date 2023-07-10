package com.gildedrose.item.quality.updater;

import com.gildedrose.Item;
import com.gildedrose.item.quality.QualityUpdater;

public class QualityUpdaterDefault implements QualityUpdater {
    public void updateQuality(final Item item) {
        decreaseQuality(item);
        updateSellIn(item);
        if (item.sellIn < MIM_LIMIT_SELL_IN) {
            decreaseQuality(item);
        }
    }

    private void decreaseQuality(final Item item) {
        if (item.quality > MIM_LIMIT_QUALITY) {
            item.quality--;
        }
    }

    private void updateSellIn(final Item item) {
        item.sellIn--;
    }
}
