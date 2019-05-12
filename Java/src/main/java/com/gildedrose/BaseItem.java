package com.gildedrose;

import com.sun.xml.internal.rngom.parse.host.Base;

public abstract class BaseItem implements ItemInterface {
    public Item item;
    public static final int MAX_QUAILITY_FOR_AN_ITEM = 50;

    public BaseItem() {}

    public BaseItem(Item item) {
        this.item=item;
    }

    protected boolean itemHasExpired() {
        boolean condition;
        if (item.sellIn < 0) {
            condition = true;
        } else {
            condition = false;
        }
        return condition;
    }

    protected void increaseQualityBy(int factor) {
        item.quality += factor;
        qualityOfAnItemIsNotMoreThan(MAX_QUAILITY_FOR_AN_ITEM);
    }

    protected void decreaseQualityBy(int factor) {
        item.quality -= factor;
        qualityOfAnItemIsNeverNegative();
    }

    private void qualityOfAnItemIsNotMoreThan(int limit) {
        if (item.quality > limit) {
            item.quality = limit;
        }
    }

    private void qualityOfAnItemIsNeverNegative() {
        if (item.quality < 0) {
            item.quality = 0;
        }
    }

}
