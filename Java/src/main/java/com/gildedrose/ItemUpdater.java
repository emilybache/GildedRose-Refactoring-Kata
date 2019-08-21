package com.gildedrose;

public abstract class ItemUpdater {
    static int HIGHEST_QUALITY = 50;
    static int MIN_QUALITY = 0;
    static int DEGRADE_NORMAL = -1;
    static int DEGRADE_TWICE_AS_FAST = -2;
    static int INCREASE_NORMAL = 1;
    static int INCREASE_TWICE_AS_FAST = 2;
    static int INCREASE_THRICE_AS_FAST = 3;

    void updateStateFor(Item item){
        updateSellIn(item);
        updateQuality(item);
    }

    private void updateQuality(Item item) {
        if (canUpdateQuality(item)) {
            item.quality = Math.max(getNewQuality(item), 0);
        }
    }
    abstract void updateSellIn(Item item);
    abstract boolean canUpdateQuality(Item item);
    abstract int getUpdateValue(Item item);

    private int getNewQuality(Item item){
        return Math.min(item.quality + getUpdateValue(item), HIGHEST_QUALITY);
    }
}
