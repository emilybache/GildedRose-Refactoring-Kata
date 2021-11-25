package com.gildedrose.items;

import com.gildedrose.main.Item;
import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;

public class BackstagePassItem implements ItemType {

    private final ItemHandler item;

    public BackstagePassItem(Item item) {
        this.item = new ItemHandler(item);
    }

    @Override
    public void updateQuality() {
        item.decrementSellInDate();
        if (item.sellInMoreThan10Days()) {
            item.incrementQuality();
        } else if (item.sellInLessThan10Days()) {
            item.incrementQualityBy2();
        } else if (item.sellInLessThan5Days()) {
            item.incrementQualityBy3();
        } else {
            item.makeQualityZero();
        }
    }

}
