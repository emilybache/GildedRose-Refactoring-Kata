package com.gildedrose;

public class BackstagePassUpdater extends CustomItemUpdater {

    @Override
    void updateSellIn(Item item) {
        item.sellIn -=1;
    }

    @Override
    boolean canUpdateQuality(final Item item) {
        return item.quality < HIGHEST_QUALITY && item.quality > MIN_QUALITY;
    }

    @Override
    int getUpdateValue(final Item item) {
        if (sellByDateLessThan(item, 0)) {
            return -item.quality;
        } else if (sellByDateLessThan(item, 6)) {
            return INCREASE_THRICE_AS_FAST;
        } else if (sellByDateLessThan(item, 11) ) {
            return INCREASE_TWICE_AS_FAST;
        }
        return INCREASE_NORMAL;
    }

    private static boolean sellByDateLessThan(final Item item, final int remainingDayCount) {
        return item.sellIn < remainingDayCount;
    }
}
