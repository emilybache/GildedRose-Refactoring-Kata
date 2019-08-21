package com.gildedrose;

class StandardItemUpdater extends ItemUpdater {

    StandardItemUpdater() {
    }

    @Override
    void updateSellIn() {
        item.sellIn -= 1;
    }

    @Override
    boolean canUpdateQuality() {
        return item.quality <= HIGHEST_QUALITY && item.quality > MIN_QUALITY;
    }

    @Override
    int getUpdateValue() {
        if (item.sellIn < 0) {
            return DEGRADE_NORMAL * 2;
        } else {
            return DEGRADE_NORMAL;
        }
    }

    @Override
    int getNewQuality() {
        return Math.min(item.quality + getUpdateValue(), HIGHEST_QUALITY);
    }
}
