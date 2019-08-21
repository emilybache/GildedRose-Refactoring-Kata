package com.gildedrose;

class AgedBrieUpdater extends CustomItemUpdater {

    AgedBrieUpdater() {
    }

    @Override
    void updateSellIn() {
        item.sellIn -= 1;
    }

    @Override
    boolean canUpdateQuality() {
        return item.quality < HIGHEST_QUALITY;
    }

    @Override
    int getUpdateValue() {
        return INCREASE_NORMAL;
    }
}
