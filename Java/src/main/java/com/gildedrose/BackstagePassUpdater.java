package com.gildedrose;

public class BackstagePassUpdater extends ItemUpdater {
    @Override
    void updateQuality(Item item) {
        if (item.sellIn < 0) {
            item.quality = 0;
        } else if (item.quality < 50) {
            item.quality += 1;

            if (item.sellIn < 11 && item.quality < 50) {
                item.quality += 1;
            }

            if (item.sellIn < 6 && item.quality < 50) {
                item.quality += 1;
            }
        }
    }

    @Override
    void updateSellIn(Item item) {
        item.sellIn -=1;
    }
}
