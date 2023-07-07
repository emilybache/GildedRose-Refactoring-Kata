package com.gildedrose.business;

import com.gildedrose.Item;

public interface ItemHandler {

    void updateItem(Item item);

    default void incrementQuality(Item item, boolean condition){
        if (condition) {
            item.quality++;
        }
    }

    default void decrementQuality(Item item, boolean condition){
        if (condition) {
            item.quality--;
        }
    }

    default void decrementSellIn(Item item){
        item.sellIn--;
    }
}
