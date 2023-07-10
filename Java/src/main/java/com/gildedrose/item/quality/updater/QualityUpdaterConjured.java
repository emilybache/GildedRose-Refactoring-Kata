package com.gildedrose.item.quality.updater;

import com.gildedrose.Item;
import com.gildedrose.item.quality.QualityUpdater;

public class QualityUpdaterConjured implements QualityUpdater {

    private static final int AMOUNT_OF_QUALITY_TO_DECREASE = 2;

    public void updateQuality(final Item item) {
        decreaseQuality(item);
        updateSellIn(item);
    }

    private void decreaseQuality(final Item item) {
        if (item.quality > MIM_LIMIT_SELL_IN) {
            item.quality -= AMOUNT_OF_QUALITY_TO_DECREASE;
        }
    }

    private void updateSellIn(final Item item) {
        item.sellIn--;
    }
}
