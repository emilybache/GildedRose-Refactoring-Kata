package com.gildedrose;

public abstract class ItemUpdater {
    static int HIGHEST_QUALITY = 50;
    static int DEGRADE_NORMAL = -1;
    static int DEGRADE_TWICE_AS_FAST = -2;
    static int INCREASE_NORMAL = 1;
    static int INCREASE_TWICE_AS_FAST = 2;
    static int INCREASE_THRICE_AS_FAST = 3;

    public void updateStateFor(Item item){
        updateSellIn(item);
        updateQuality(item);
    }
    abstract void updateQuality(Item item);
    abstract void updateSellIn(Item item);
    abstract boolean canUpdateQuality(Item item);
    abstract int getDegradeValue(Item item);

    public int getHighestQuality() {
        return HIGHEST_QUALITY;
    }
}
