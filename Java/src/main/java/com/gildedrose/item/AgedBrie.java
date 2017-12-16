package com.gildedrose.item;

public class AgedBrie implements CustomisedItem {

    public Item item;

    public AgedBrie(Item item) {
        this.item = item;
    }

    public void updateState() {
        item.sellIn -= 1;
        item.quality += 1;
    }
}
