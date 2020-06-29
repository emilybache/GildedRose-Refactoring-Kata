package com.gildedrose.item;

import com.gildedrose.Item;

public class BackstagePassesGRItem extends GRItem {

    BackstagePassesGRItem(Item item) {
        super(item);
    }

    @Override
    public void updateQuality() {
        if (item.quality < MAX_QUALITY) {
            item.quality = item.quality + calculateQualityIncrement();
        }

        item.sellIn = item.sellIn - 1;

        if (item.sellIn < 0) {
            item.quality = MIN_QUALITY;
        } else if (item.quality > MAX_QUALITY) {
            item.quality = MAX_QUALITY;
        }
    }

    private int calculateQualityIncrement() {
        if (item.sellIn < 6) {
            return 3;
        } else if (item.sellIn < 11) {
            return 2;
        } else {
            return 1;
        }
    }
}
