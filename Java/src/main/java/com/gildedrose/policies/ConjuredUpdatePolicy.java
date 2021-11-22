package com.gildedrose.policies;

import com.gildedrose.Item;

class ConjuredUpdatePolicy extends UpdatePolicy {
    ConjuredUpdatePolicy(Item item) {
        super(item);
    }

    @Override
    void updateItem() {
        super.updateItem();
        decreaseQuality();
    }
}
