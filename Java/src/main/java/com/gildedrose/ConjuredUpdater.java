package com.gildedrose;

class ConjuredUpdater extends StandardItemUpdater {

    ConjuredUpdater() {
    }

    @Override
    int getUpdateValue() {
        if (item.sellIn < 0) {
            return DEGRADE_TWICE_AS_FAST * 2;
        } else {
            return DEGRADE_TWICE_AS_FAST;
        }
    }
}
