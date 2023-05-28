package com.gildedrose.model;

import com.gildedrose.Item;

public class BaseItem {
    private final Item item;
    public BaseItem(Item item) {
        this.item = item;
    }

    public int getSellIn() {
        return item.sellIn;
    }

    public void setSellIn(int sellIn) {
        item.sellIn = sellIn;
    }

    public void update() {
        decrementSellIn();
        updateQuality();
    }

    private void decrementSellIn() {
        setSellIn(getSellIn() - 1);
    }

    public int getQuality() {
        return item.quality;
    }

    public void setQuality(int quality) {
        item.quality = quality;
    }

    private void updateQuality() {
        setQuality(getQuality() - 1);
    }

    protected boolean isExpired() {
        return item.sellIn < 0;
    }
}
