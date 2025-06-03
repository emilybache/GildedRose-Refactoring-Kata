package com.gildedrose;

import static java.lang.Math.max;
import static java.lang.Math.min;

abstract class UpdatableItem {
    protected static final int DOUBLE_VALUE_CHANGE = 2;
    protected static final int STANDARD_VALUE_CHANGE = 1;
    protected static final int QUALITY_MAX = 50;
    protected static final int QUALITY_MIN = 0;
    protected static final int ZERO_SELL_IN = 0;

    protected Item item;

    protected UpdatableItem(Item item) {
        this.item = item;
    }

    public abstract void update();

    protected void incrementQuality(int incrementAmount) {
        item.quality = min(QUALITY_MAX, item.quality + incrementAmount);
    }

    protected void setZeroQuality() {
        item.quality = QUALITY_MIN;
    }

    protected void decrementQuality(int decrementAmount) {
        item.quality = max(QUALITY_MIN, item.quality - decrementAmount);
    }

    protected void decrementSellIn() {
        item.sellIn--;
    }

    protected boolean isOutdated() {
        return item.sellIn < ZERO_SELL_IN;
    }
}

