package com.gildedrose;

/**
 * Class for a regular item with business rule for normal item:
 *  If the item expired => reduce the quality by 2 else reduce by 1
 *  Quality for an item is never negative
 *  Quality for an item is not greater than the constant MAX_QUAILITY_FOR_AN_ITEM
 */
public class NormalItem {
    public static final int MAX_QUAILITY_FOR_AN_ITEM = 50;
    public Item item;

    public NormalItem() {

    }

    public NormalItem(Item item) {
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
