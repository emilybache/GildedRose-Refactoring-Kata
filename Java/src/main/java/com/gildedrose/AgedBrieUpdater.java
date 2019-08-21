package com.gildedrose;

public class AgedBrieUpdater extends ItemUpdater {
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
        return item.quality < HIGHEST_QUALITY;
    }

    @Override
    int getDegradeValue(Item item) {
        return INCREASE_NORMAL;
    }
}
