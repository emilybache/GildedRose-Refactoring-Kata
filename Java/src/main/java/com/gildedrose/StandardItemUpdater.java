package com.gildedrose;

public class StandardItemUpdater extends ItemUpdater {

    @Override
    void updateQuality(Item item) {
        if (canUpdateQuality(item)) {
            item.quality += getDegradeValue(item);
        }
    }

    @Override
    void updateSellIn(Item item) {
        item.sellIn -= 1;
    }

    @Override
    boolean canUpdateQuality(Item item) {
        return item.quality < HIGHEST_QUALITY && item.quality > 0;
    }

    @Override
    int getDegradeValue(Item item) {
        if (item.sellIn < 0) {
            return item.quality + DEGRADE_TWICE_AS_FAST < 0 ? DEGRADE_NORMAL : DEGRADE_TWICE_AS_FAST;
        } else {
            return DEGRADE_NORMAL;
        }
    }
}
