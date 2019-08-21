package com.gildedrose;

public class AgedBrieUpdater extends CustomItemUpdater {

    @Override
    void updateSellIn(Item item) {
        item.sellIn -= 1;
    }

    @Override
    boolean canUpdateQuality(final Item item) {
        return item.quality < HIGHEST_QUALITY;
    }

    @Override
    int getUpdateValue(final Item item) {
        return INCREASE_NORMAL;
    }
}
