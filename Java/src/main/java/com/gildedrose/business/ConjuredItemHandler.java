package com.gildedrose.business;

import com.gildedrose.Item;

public class ConjuredItemHandler implements ItemHandler {

    @Override
    public void updateItem(Item item) {
        if (item.quality > 0) {
            decrementQuality(item);
            decrementQuality(item);
        }
        decrementSellIn(item);

        if (hasExpired(item) && item.quality > 0) {
            decrementQuality(item);
            decrementQuality(item);
        }
    }
}
