package com.gildedrose;

public class ConjuredUpdater extends ItemUpdater {
    @Override
    void updateQuality(Item item) {
        if (item.quality > 0) {
            item.quality -= 2;
            if (item.quality < 0 )
                item.quality =0;
        }
    }

    @Override
    void updateSellIn(Item item) {
        item.sellIn -= 1;
    }
}
