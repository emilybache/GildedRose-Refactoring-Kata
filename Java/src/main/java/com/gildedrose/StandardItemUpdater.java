package com.gildedrose;

public class StandardItemUpdater extends ItemUpdater{

    @Override
    void updateQuality(Item item) {
        if (item.quality > 0) {
            item.quality  -= 1;
        }
    }

    @Override
    void updateSellIn(Item item) {
        item.sellIn -= 1;
    }
}
