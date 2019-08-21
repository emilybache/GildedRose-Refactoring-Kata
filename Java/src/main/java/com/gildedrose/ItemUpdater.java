package com.gildedrose;

public abstract class ItemUpdater {
    public void updateStateFor(Item item){
        updateSellIn(item);
        updateQuality(item);
    }
    abstract void updateQuality(Item item);
    abstract void updateSellIn(Item item);
}
