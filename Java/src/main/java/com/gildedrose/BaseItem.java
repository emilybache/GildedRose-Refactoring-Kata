package com.gildedrose;


/**
 * Common functions used by all items (inside the package com.gildedrose.items)
 */
public abstract class BaseItem {
    public Item item;
    public static final int MAX_QUAILITY_FOR_AN_ITEM = 50;

    public BaseItem() {}

    public BaseItem(Item item) {
        this.item=item;
    }

    /**
     * Check if item is expired
     *
     * @return true, false
     */
    protected boolean itemHasExpired() {
        boolean condition;
        if (item.sellIn < 0) {
            condition = true;
        } else {
            condition = false;
        }
        return condition;
    }

    /**
     * Increase quality for each item
     *
     * @param factor, number to increase quality
     */
    protected void increaseQualityBy(int factor) {
        item.quality += factor;
        qualityOfAnItemIsNotMoreThan(MAX_QUAILITY_FOR_AN_ITEM);
    }

    /**
     * Decrease quality for each item
     *
     * @param factor, number to decrease quality
     */
    protected void decreaseQualityBy(int factor) {
        item.quality -= factor;
        qualityOfAnItemIsNeverNegative();
    }

    /**
     * Quality can never be increased above a limit
     *
     * @param limit, max item quality
     */
    private void qualityOfAnItemIsNotMoreThan(int limit) {
        if (item.quality > limit) {
            item.quality = limit;
        }
    }

    /**
     * Quality can never be negative
     */
    private void qualityOfAnItemIsNeverNegative() {
        if (item.quality < 0) {
            item.quality = 0;
        }
    }
}
