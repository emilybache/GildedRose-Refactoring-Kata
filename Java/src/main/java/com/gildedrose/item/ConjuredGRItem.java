package com.gildedrose.item;

import com.gildedrose.Item;

public class ConjuredGRItem extends GRItem {

    public ConjuredGRItem(Item item) {
        super(item);
    }

    @Override
    public void updateQuality() {
        if (item.quality > MIN_QUALITY) {
            item.quality = item.quality - 2;
        }
        item.sellIn = item.sellIn - 1;

        if (item.sellIn < 0 && item.quality > MIN_QUALITY) {
            item.quality = item.quality - 2;
        }

        if (item.quality < MIN_QUALITY) {
            item.quality = MIN_QUALITY;
        }
    }
}
