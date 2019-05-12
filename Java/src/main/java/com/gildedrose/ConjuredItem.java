package com.gildedrose;

/**
 * Class for the item ConjuredItem inherited from NormalItem
 */
public class ConjuredItem extends NormalItem {
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
