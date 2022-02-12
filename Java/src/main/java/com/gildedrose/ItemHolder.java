package com.gildedrose;

public abstract class ItemHolder {
    Item item;

    public ItemHolder(Item item){
        this.item = item;
    }

    public void update() {
        this.updateQuality();
        this.updateSellIn();
    }
    abstract void updateQuality();

    abstract void updateSellIn();
}
