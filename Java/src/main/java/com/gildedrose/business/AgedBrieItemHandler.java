package com.gildedrose.business;

import com.gildedrose.Item;

public class AgedBrieItemHandler implements ItemHandler {

    @Override
    public void updateItem(Item item) {
        if (item.quality < 50) {
            incrementQuality(item);
        }
        decrementSellIn(item);

        if (hasExpired(item) && item.quality < 50) {
            incrementQuality(item);
        }
    }
}

