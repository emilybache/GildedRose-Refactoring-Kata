package com.gildedrose.items;

import com.gildedrose.Item;

public class AgingGildedRoseItem extends AbstractGildedRoseItem {

    final static int DOUBLE_QUALITY_INCREASE_DAYS = 10;
    final static int TRIPLE_QUALITY_INCREASE_DAYS = 5;

    final Boolean hardDegradation;

    public AgingGildedRoseItem(Item item, Boolean hardDegradation) {
        super(item);
        this.hardDegradation = hardDegradation;
    }

    @Override
    public GildedRoseItem updateQuality() {
        item.sellIn = item.sellIn - 1;

        int newQuality;

        if (getSellIn() <= 0) {
            newQuality = hardDegradation ? 0 : getQuality() - 1;
        } else if (getSellIn() <= TRIPLE_QUALITY_INCREASE_DAYS) {
            newQuality = getQuality() + 3;
        } else if (getSellIn() <= DOUBLE_QUALITY_INCREASE_DAYS) {
            newQuality = getQuality() + 2;
        } else {
            newQuality = getQuality() + 1;
        }
        item.quality = Math.min(Math.max(newQuality, 0), MAX_QUALITY);

        return this;
    }
}
