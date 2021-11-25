package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;

public class NormalItem implements ItemType {

    private final ItemHandler item;

    public NormalItem(Item item) {
        this.item = new ItemHandler(item);
    }

    @Override
    public void updateQuality() {
        item.decrementSellInDate();
        if (item.qualityIsHigherThanZero()) {
            if (item.beforeSellInDate()) {
                item.decrementQuality();
            } else {
                item.decrementQualityBy2();
            }
        }
    }

}
