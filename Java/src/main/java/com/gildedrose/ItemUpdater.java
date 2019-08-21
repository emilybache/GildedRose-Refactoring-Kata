package com.gildedrose;

abstract class ItemUpdater {
    static int HIGHEST_QUALITY = 50;
    static int MIN_QUALITY = 0;
    static int DEGRADE_NORMAL = -1;
    static int DEGRADE_TWICE_AS_FAST = -2;
    static int INCREASE_NORMAL = 1;
    static int INCREASE_TWICE_AS_FAST = 2;
    static int INCREASE_THRICE_AS_FAST = 3;

    Item item;

    ItemUpdater() {}

    void updateStateFor(){
        updateSellIn();
        updateQuality();
    }

    private void updateQuality() {
        if (canUpdateQuality()) {
            item.quality = Math.max(getNewQuality(), MIN_QUALITY);
        }
    }
    abstract void updateSellIn();
    abstract boolean canUpdateQuality();
    abstract int getUpdateValue();
    abstract int getNewQuality();

    public void setItem(Item item) {
        this.item = item;
    }
}
