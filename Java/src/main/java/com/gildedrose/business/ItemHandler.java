package com.gildedrose.business;

import com.gildedrose.Item;

public interface ItemHandler {

    void updateItem(Item item);

    default void incrementQuality(Item item){
        item.quality++;
    }

    default void decrementQuality(Item item){
        item.quality--;
    }

    default void decrementSellIn(Item item){
        item.sellIn--;
    }

    default boolean hasExpired(Item item) {
        return item.sellIn < 0;
    }
}
