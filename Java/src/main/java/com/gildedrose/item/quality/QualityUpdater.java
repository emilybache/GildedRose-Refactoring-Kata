package com.gildedrose.item.quality;

import com.gildedrose.Item;

public interface QualityUpdater {
    int MAX_LIMIT_QUALITY = 50;
    int MIM_LIMIT_QUALITY = 0;
    int MIM_LIMIT_SELL_IN = 0;
    void updateQuality(final Item item);
}

