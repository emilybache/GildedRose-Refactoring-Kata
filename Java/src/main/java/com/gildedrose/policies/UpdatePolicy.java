package com.gildedrose.policies;

import com.gildedrose.Item;

class UpdatePolicy {

    private final int MAX_QUALITY = 50;
    private final int MIN_QUALITY = 0;

    protected Item item;

    UpdatePolicy(Item item) {
        this.item = item;
    }

    void updateItem() {
        decreaseSellIn();
        decreaseQuality();
    }

    void decreaseSellIn() {
        item.sellIn -= 1;
    }

    void decreaseQuality() {
        item.quality = item.quality <= MIN_QUALITY ? MIN_QUALITY : isExpired() ? item.quality - 2 : item.quality - 1;
    }

    void increaseQuality() {
        item.quality = item.quality >= MAX_QUALITY ? MAX_QUALITY : isExpired() ? item.quality + 2 : item.quality + 1;
    }

    private boolean isExpired() {
        return item.sellIn < 0;
    }
}
