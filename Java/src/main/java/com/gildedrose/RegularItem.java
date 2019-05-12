package com.gildedrose;

public class RegularItem {
    public Item item;

    public RegularItem() {

    }

    public RegularItem(Item item) {
        this.item = item;
    }

    public void updateQuality() {
        if (itemHasExpired()) {
            decreaseQualityBy(2);
        } else {
            decreaseQualityBy(1);
        }
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
        qualityOfAnItemIsNotMoreThan(50);
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
