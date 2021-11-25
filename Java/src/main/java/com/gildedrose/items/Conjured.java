package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;

public class Conjured implements ItemType {

    private final ItemHandler item;

    public Conjured(Item item) {
        this.item = new ItemHandler(item);
    }

    @Override
    public void updateQuality() {
        item.decrementSellInDate();
        if (item.qualityIsHigherThanZero()) {
            if (item.sellInDatePasses()) {
                item.decrementQualityBy4();
            } else {
                item.decrementQuality();
            }
        }
    }

}
