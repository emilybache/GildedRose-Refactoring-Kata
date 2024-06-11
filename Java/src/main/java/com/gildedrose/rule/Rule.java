package com.gildedrose.rule;

import com.gildedrose.Item;

public interface Rule {
    boolean match(Item item);

    void apply(Item item);

    default void increaseQuality(Item item) {
        if (item.quality < 50) {
            item.quality += 1;
        }
    }

    default void decreaseQuality(Item item) {
        if (item.quality > 0) {
            item.quality -= 1;
        }
    }
}
