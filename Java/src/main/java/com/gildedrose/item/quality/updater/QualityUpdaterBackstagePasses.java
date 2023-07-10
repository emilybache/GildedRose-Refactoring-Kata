package com.gildedrose.item.quality.updater;

import com.gildedrose.Item;
import com.gildedrose.item.quality.QualityUpdater;

public class QualityUpdaterBackstagePasses implements QualityUpdater {

    private static final int FIRST_SELL_IN_LIMIT = 6;
    private static final int SECOND_SELL_IN_LIMIT = 11;

    public void updateQuality(final Item item) {
        increaseQuality(item);
        if (item.sellIn < FIRST_SELL_IN_LIMIT) {
            increaseQuality(item);
        }
        if (item.sellIn < SECOND_SELL_IN_LIMIT) {
            increaseQuality(item);
        }
        item.sellIn--;
        if (item.sellIn < MIM_LIMIT_SELL_IN) {
            item.quality = MIM_LIMIT_QUALITY;
        }
    }

    private void increaseQuality(final Item item) {
        if (item.quality < MAX_LIMIT_QUALITY) {
            item.quality++;
        }
    }
}
