package com.gildedrose;

/**
 * Class for Aged Brie item inherited from NormalItem
 */
public class AgedBrie extends NormalItem {

    public AgedBrie(Item item) {
        this.item=item;
    }

    public void updateQuaility() {
        if (itemHasExpired()) {
            increaseQualityBy(2);
        } else {
            increaseQualityBy(1);
        }
    }
}
