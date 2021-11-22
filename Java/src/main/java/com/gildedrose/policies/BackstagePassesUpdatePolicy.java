package com.gildedrose.policies;

import com.gildedrose.Item;

class BackstagePassesUpdatePolicy extends UpdatePolicy {
    BackstagePassesUpdatePolicy(Item item) {
        super(item);
    }

    @Override
    void updateItem() {
        decreaseSellIn();

        var sellIn = item.sellIn;

        if (sellIn < 0) {
            item.quality = 0;
            return;
        }

        increaseQuality();

        if (sellIn < 10) {
            increaseQuality();
        }

        if (sellIn < 5) {
            increaseQuality();
        }
    }
}
