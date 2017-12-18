package com.gildedrose.item;

public class StandardItem implements CustomisedItem {

    private final Item item;

    public StandardItem(Item item) {
        this.item = item;
    }

    public void updateState() {
        item.sellIn -= 1;
        if (item.sellIn > 0) {
            item.quality -= 1;
        } else {
            item.quality -= 2;
        }
    }


}
