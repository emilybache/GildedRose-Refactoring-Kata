package com.gildedrose;

public class StandardItemUpdater extends ItemUpdater {

    @Override
    void updateSellIn(Item item) {
        item.sellIn -= 1;
    }

    @Override
    boolean canUpdateQuality(Item item) {
        return item.quality <= HIGHEST_QUALITY && item.quality > MIN_QUALITY;
    }

    @Override
    int getUpdateValue(Item item) {
        if (item.sellIn < 0) {
            return DEGRADE_NORMAL * 2;
        } else {
            return DEGRADE_NORMAL;
        }
    }
}
