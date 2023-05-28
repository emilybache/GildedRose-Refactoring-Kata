package com.gildedrose.model;

import com.gildedrose.Item;

public class BaseItem {
    private final Item item;
    protected final static int MIN_QUALITY = 0;
    public BaseItem(Item item) {
        validate(item);
        this.item = item;
    }

    private void validate(Item item) {
        if(item.quality < MIN_QUALITY) {
            throw new IllegalArgumentException(
                "Item has a quality out of accepted boundaries. Minimum acceptable quality is "
                    +  " but received "
                    + item.quality
            );
        }
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
        item.quality = Math.max(quality, MIN_QUALITY);
    }

    private void updateQuality() {
        setQuality(getQuality() + qualityChange());
    }

    protected boolean isExpired() {
        return item.sellIn < 0;
    }

    private int qualityChange() {
        return isExpired() ? -2 : -1;
    }
}
