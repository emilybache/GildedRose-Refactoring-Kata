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
        return item.quality < HIGHEST_QUALITY;
    }

    @Override
    int getUpdateValue() {
        int sellIn = item.sellIn;
        if (sellInLessThan(sellIn, 0)) {
            // if sell by date has passed then the quality should be zero
            return -item.quality;
        } else if (sellInLessThan(sellIn, 6)) {
            return INCREASE_THRICE_AS_FAST;
        } else if (sellInLessThan(sellIn, 11) ) {
            return INCREASE_TWICE_AS_FAST;
        }
        return INCREASE_NORMAL;
    }

    private static boolean sellInLessThan(final int sellIn, final int remainingDayCount) {
        return sellIn < remainingDayCount;
    }
}
