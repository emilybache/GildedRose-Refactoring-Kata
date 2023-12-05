package com.gildedrose.items;

import com.gildedrose.Item;

public class StandardGildedRoseItem extends AbstractGildedRoseItem {

    public static final int STANDARD_DEGRADATION = 1;

    private final int degradation;

    public StandardGildedRoseItem(Item item) {
        super(item);
        item.quality = Math.min(item.quality, MAX_QUALITY);
        degradation = STANDARD_DEGRADATION;
    }

    public StandardGildedRoseItem(Item item, int degradation) {
        super(item);
        item.quality = Math.min(item.quality, MAX_QUALITY);
        this.degradation = degradation;
    }

    public GildedRoseItem updateQuality() {
        item.sellIn = item.sellIn - 1;
        item.quality = item.sellIn < 0 ? Math.max(item.quality - degradation * 2, 0) : Math.max(item.quality - degradation, 0);
        return this;
    }
}
