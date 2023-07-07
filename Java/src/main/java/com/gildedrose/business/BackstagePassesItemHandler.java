package com.gildedrose.business;

import com.gildedrose.Item;

public class BackstagePassesItemHandler implements ItemHandler{
    @Override
    public void updateItem(Item item) {
        incrementQuality(item, item.quality < 50);
        incrementQuality(item, item.sellIn < 11 && item.quality < 50);
        incrementQuality(item, item.sellIn < 6 && item.quality < 50);
        decrementSellIn(item);
        initQuality(item, item.sellIn < 0);
    }

    private void initQuality(Item item, boolean condition) {
        if (condition) {
            item.quality = 0;
        }
    }
}
