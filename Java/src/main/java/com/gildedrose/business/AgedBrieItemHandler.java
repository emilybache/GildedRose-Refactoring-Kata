package com.gildedrose.business;

import com.gildedrose.Item;

public class AgedBrieItemHandler implements ItemHandler {

    @Override
    public void updateItem(Item item) {
        incrementQuality(item,item.quality < 50);
        decrementSellIn(item);
        incrementQuality(item,item.sellIn < 0 && item.quality < 50);
    }

}

