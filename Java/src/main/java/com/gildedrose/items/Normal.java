package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;

public class Normal implements ItemType {

    private final ItemHandler item;

    public Normal(Item item) {
        this.item = new ItemHandler(item);
    }

    @Override
    public void updateQuality() {
        item.decrementSellInDate();
        if (item.qualityIsHigherThanZero()) {
            if (item.sellInDatePasses()) {
                item.decrementQualityBy2();
            } else {
                item.decrementQuality();
            }
        }
    }

}
