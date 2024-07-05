package com.gildedrose;

public class BackstagePassesItemUpdater extends ItemUpdater {

    @Override
    public void update(Item item) {
        updateQuality(item, getIncrementValue());
        if (item.sellIn <= 10) {
            updateQuality(item, getIncrementValue());
        }
        if (item.sellIn <= 5) {
            updateQuality(item, getIncrementValue());
        }
        updateSellIn(item);
        if (isExpired(item)) {
            item.quality = 0;
        }
    }

    @Override
    int getIncrementValue() {
        return 1;
    }
}
