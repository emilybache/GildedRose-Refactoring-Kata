package com.gildedrose.strategy;

import com.gildedrose.Item;

/**
 * Defines the strategy for updating the quality and sellIn of an item.
 */
public interface ItemStrategy {
    int MINIMUM_QUALITY = 0;
    int MAXIMUM_QUALITY = 50;
    int DEGRADATION_RATE = 1;
    int INCREASE_RATE = 1;

    /**
     * Updates the quality of a normal item. Degradation rate is doubled after sellIn.
     * Specialized item types should override this method.
     *
     * @param item The item to update
     */
    default void updateQuality(Item item) {
        item.sellIn--;
        int decrement = item.sellIn < 0 ? 2 * DEGRADATION_RATE : DEGRADATION_RATE;
        decreaseQuality(item, decrement);
    }

    default void increaseQuality(Item item, int amount) {
        item.quality = Math.min(MAXIMUM_QUALITY, item.quality + amount);
    }

    default void decreaseQuality(Item item, int amount) {
        item.quality = Math.max(MINIMUM_QUALITY, item.quality - amount);
    }
}
