package com.gildedrose;

public class BackstagePassUpdater extends ItemUpdater {
    @Override
    void updateQuality(Item item) {
        if (sellByDateLessThan(item, 0)) {
            item.quality = 0;
        } else if (canUpdateQuality(item)) {
            item.quality += getDegradeValue(item);
        }
    }

    @Override
    void updateSellIn(Item item) {
        item.sellIn -=1;
    }

    @Override
    boolean canUpdateQuality(final Item item) {
        return item.quality < HIGHEST_QUALITY;
    }

    @Override
    int getDegradeValue(final Item item) {
        if (sellByDateLessThan(item, 6)) {
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
