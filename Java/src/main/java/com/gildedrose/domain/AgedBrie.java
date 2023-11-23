package com.gildedrose.domain;

import com.gildedrose.Item;

public class AgedBrie extends InventoryItem {

    public AgedBrie(Item item) {
        setName(item.name);
        setSellIn(item.sellIn);
        setQuality(item.quality);
    }

    @Override
    public int handleQuality() {
        return increaseQualityBelowMaximum();
    }

    @Override
    public int qualityDecreaseAmount() {
        return 1;
    }

    @Override
    public int handleQualityAfterSellIn() {
        return increaseQualityBelowMaximum();
    }
}
