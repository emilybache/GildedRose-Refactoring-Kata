package com.gildedrose.domain;

import com.gildedrose.Item;

public class DefaultItem extends InventoryItem {

    public DefaultItem(Item item) {
        setName(item.name);
        setSellIn(item.sellIn);
        setQuality(item.quality);
    }

    @Override
    public int qualityDecreaseAmount() {
        return 1;
    }

    @Override
    public int handleQualityAfterSellIn() {
        return decreaseQualityAboveZero();
    }
}
