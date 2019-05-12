package com.gildedrose;

public class ConjuredItem extends RegularItem {
    public ConjuredItem(Item item) {
        this.item=item;
    }

    public void updateQuality() {
        decreaseQualityBy(1);
        if (itemHasExpired()) {
            decreaseQualityBy(1);
        }
    }
}
