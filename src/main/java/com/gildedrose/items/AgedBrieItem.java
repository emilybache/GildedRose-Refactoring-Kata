package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

public class AgedBrieItem implements ItemType {

    private final ItemHandler item;

    public AgedBrieItem(Item item) {
        this.item = new ItemHandler(item);
    }

    @Override
    public void updateQuality() {
        item.decrementSellInDate();
        if (item.beforeSellInDate()) {
            item.incrementQuality();
        } else {
            item.incrementQualityBy2();
        }
    }

}
