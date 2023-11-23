package com.gildedrose.domain;

import com.gildedrose.Item;

public class ConjuredItem extends InventoryItem {

    public ConjuredItem(Item item) {
        setName(item.name);
        setSellIn(item.sellIn);
        setQuality(item.quality);
    }

    @Override
    public int qualityDecreaseAmount() {
        return 2;
    }

    @Override
    public int handleQualityAfterSellIn() {
        quality = decreaseQualityAboveZero();
        return quality;
    }
}
