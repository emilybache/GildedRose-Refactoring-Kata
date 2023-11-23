package com.gildedrose.domain;

import com.gildedrose.Item;
import com.gildedrose.service.InventoryItem;

public class ConjuredItem extends InventoryItem {

    public ConjuredItem(Item item) {
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
        return 2;
    }

    @Override
    public int handleQualityAfterSellIn() {
        quality = decreaseQualityAboveZero();
        return quality;
    }
}
