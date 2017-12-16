package com.gildedrose.item;

public class BackstagePassesItem implements CustomisedItem {

    public Item item;

    public BackstagePassesItem(Item item) {
        this.item = item;
    }

    public void updateState() {
        item.sellIn -= 1;
        if (item.sellIn >= 11) {
            item.quality += 1;
        } else if (item.sellIn > 5) {
            item.quality += 2;
        } else if (item.sellIn > 0) {
            item.quality += 3;
        } else {
            item.quality = 0;
        }
    }
}
