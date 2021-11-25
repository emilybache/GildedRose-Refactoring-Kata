package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;

public class BackstagePass implements ItemType {

    private final ItemHandler item;

    public BackstagePass(Item item) {
        this.item = new ItemHandler(item);
    }

    @Override
    public void updateQuality() {
        item.decrementSellInDate();
        if (item.sellInDaysMoreThan10Days()) {
            item.incrementQuality();
        } else if (item.sellInLessThan10Days()) {
            item.incrementQualityByTwo();
        } else if (item.sellInLessThan5Days()) {
            item.incrementQualityBy3();
        } else {
            item.makeQualityZero();
        }
    }

}
