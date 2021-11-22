package com.gildedrose.policies;

import com.gildedrose.Item;

class AgedBrieUpdatePolicy extends UpdatePolicy {
    AgedBrieUpdatePolicy(Item item) {
        super(item);
    }

    @Override
    void updateItem() {
        decreaseSellIn();
        increaseQuality();
    }
}
