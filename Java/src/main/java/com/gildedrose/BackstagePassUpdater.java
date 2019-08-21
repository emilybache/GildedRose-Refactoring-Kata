package com.gildedrose;

class BackstagePassUpdater extends CustomItemUpdater {

    BackstagePassUpdater() {
    }

    @Override
    void updateSellIn() {
        item.sellIn -=1;
    }

    @Override
    boolean canUpdateQuality() {
        return item.quality < HIGHEST_QUALITY && item.quality > MIN_QUALITY;
    }

    @Override
    int getUpdateValue() {
        if (sellByDateLessThan(item, 0)) {
            // if sell by date has passed then the quality should be zero
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
