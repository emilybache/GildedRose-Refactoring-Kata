package com.gildedrose.item.quality.updater;

import com.gildedrose.Item;
import com.gildedrose.item.quality.QualityUpdater;

public class QualityUpdaterSulfuras implements QualityUpdater {

    public void updateQuality(final Item item) {
        item.sellIn--;
    }
}
