package com.gildedrose;

public class AgedBrie extends RegularItem {

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
