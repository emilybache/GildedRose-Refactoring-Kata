package com.gildedrose.items;

import com.gildedrose.Item;

public class StandardGildedRoseItem extends AbstractGildedRoseItem {

    private final static int MAX_QUALITY = 50;

    public StandardGildedRoseItem(Item item) {
        super(item);
        item.quality = Math.min(item.quality, MAX_QUALITY);
    }

    public GildedRoseItem updateQuality() {
        item.sellIn = item.sellIn - 1;
        item.quality = item.sellIn < 0 ? Math.max(item.quality - 2, 0) : Math.max(item.quality - 1, 0);
        return this;
    }
}
