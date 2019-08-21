package com.gildedrose;

public class AgedBrieUpdater extends ItemUpdater {
    @Override
    void updateQuality(Item item) {
        if (item.quality < 50) {
            item.quality += 1;
        }
    }

    @Override
    void updateSellIn(Item item) {
        item.sellIn -=1;
    }
}
