package com.gildedrose.domain;

import com.gildedrose.Item;

public class AgedBrie extends InventoryItem {

    public AgedBrie(Item item) {
        setName(item.name);
        setSellIn(item.sellIn);
        setQuality(item.quality);
    }

    @Override
    public boolean qualityDecreaseInverted() {
        return true;
    }

    @Override
    public int qualityDecreaseAmount() {
        return 1;
    }

    @Override
    public int handleQualityAfterSellIn() {
        quality = increaseQualityBelowMaximum();
        return quality;
    }
}
