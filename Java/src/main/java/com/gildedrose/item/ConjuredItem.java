package com.gildedrose.item;

public class ConjuredItem implements CustomisedItem {

    private final Item item;

    public ConjuredItem(Item item) {
        this.item = item;
    }

    public void updateState() {
        item.sellIn -= 1;
        if (item.sellIn > 0) {
            item.quality -= 2;
        } else {
            item.quality -= 4;
        }
    }

}
