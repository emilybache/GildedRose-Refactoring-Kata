package com.gildedrose;

class StandardItemUpdater extends ItemUpdater {

    @Override
    void updateSellIn(Item item) {
        item.sellIn -= 1;
    }

    @Override
    boolean canUpdateQuality(final Item item) {
        return item.quality <= HIGHEST_QUALITY && item.quality > MIN_QUALITY;
    }

    @Override
    int getUpdateValue(final Item item) {
        if (item.sellIn < 0) {
            return DEGRADE_NORMAL * 2;
        } else {
            return DEGRADE_NORMAL;
        }
    }

    @Override
    int getNewQuality(final Item item) {
        return Math.min(item.quality + getUpdateValue(item), HIGHEST_QUALITY);
    }
}
