package com.gildedrose.item;

import com.gildedrose.Item;

public class AgedBrieGRItem extends GRItem {

    AgedBrieGRItem(Item item) {
        super(item);
    }

    @Override
    public void updateQuality() {
        if (item.quality < MAX_QUALITY) {
            item.quality = item.quality + 1;
        }
        item.sellIn = item.sellIn - 1;

        if (item.quality < MAX_QUALITY && item.sellIn < 0) {
            item.quality = item.quality + 1;
        }
    }
}
