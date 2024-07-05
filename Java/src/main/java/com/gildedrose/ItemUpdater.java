package com.gildedrose;

import static java.lang.Math.max;
import static java.lang.Math.min;

public abstract class ItemUpdater {
    public static final int QUALITY_MIN = 0;
    public static final int QUALITY_MAX = 50;

    protected void update(Item item){
        updateQuality(item, getIncrementValue());
        updateSellIn(item);
        if (isExpired(item)) {
            updateQuality(item, getIncrementValue());
        }
    }

    abstract int getIncrementValue();

    boolean isExpired(Item item) {
        return item.sellIn < 0;
    }

    void updateQuality(Item item, int increment){
        item.quality = max(QUALITY_MIN, min(QUALITY_MAX, item.quality + increment));
    }

    final void updateSellIn(Item item) {
        item.sellIn = (int) max(Integer.MIN_VALUE, min(Integer.MAX_VALUE, ((long) item.sellIn) - 1));
    }
}
