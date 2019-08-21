package com.gildedrose;

abstract class ItemUpdater {
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
            item.quality = Math.max(getNewQuality(item), MIN_QUALITY);
        }
    }
    abstract void updateSellIn(Item item);
    abstract boolean canUpdateQuality(Item item);
    abstract int getUpdateValue(Item item);
    abstract int getNewQuality(Item item);
}
