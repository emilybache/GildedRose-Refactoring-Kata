package com.gildedrose.policies;

import com.gildedrose.Item;

class SulfurasUpdatePolicy extends UpdatePolicy{
    SulfurasUpdatePolicy(Item item) {
        super(item);
    }

    @Override
    void updateItem() {
        // Do nothing
    }
}
