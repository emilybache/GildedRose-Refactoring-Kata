package com.gildedrose.domain;

import com.gildedrose.Item;

public class Legendary extends InventoryItem {

    public Legendary(Item item) {
        super();
        setName(item.name);
        setSellIn(item.sellIn);
        setQuality(item.quality);
    }

    @Override
    public boolean qualityDecreaseInverted() {
        return false;
    }

    @Override
    public int qualityDecreaseAmount() {
        return 0;
    }

    @Override
    public int handleQualityAfterSellIn() {
        return quality;
    }

    @Override
    public int decreaseQualityAboveZero() {
        return quality;
    }

    @Override
    public int reduceSellIn() {
        return sellIn;
    }
}
