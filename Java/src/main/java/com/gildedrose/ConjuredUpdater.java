package com.gildedrose;

public class ConjuredUpdater extends StandardItemUpdater {

    @Override
    int getUpdateValue(final Item item) {
        if (item.sellIn < 0) {
            return DEGRADE_TWICE_AS_FAST * 2;
        } else {
            return DEGRADE_TWICE_AS_FAST;
        }
    }
}
