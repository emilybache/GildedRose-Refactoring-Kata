package com.gildedrose.business;

import com.gildedrose.Item;

public class BackstagePassesItemHandler implements ItemHandler{
    @Override
    public void updateItem(Item item) {
        if (item.quality < 50) {
            incrementQuality(item);
            if (item.sellIn < 11 && item.quality < 50) {
                incrementQuality(item);
            }
            if (item.sellIn < 6 && item.quality < 50) {
                incrementQuality(item);
            }
        }
        decrementSellIn(item);

        if (hasExpired(item)) {
            item.quality = 0;
        }
    }
}
