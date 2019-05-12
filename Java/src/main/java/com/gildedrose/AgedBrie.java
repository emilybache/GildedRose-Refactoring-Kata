package com.gildedrose;

public class AgedBrie {
    private Item item;

    public AgedBrie(Item item) {
        this.item=item;
    }

    public void updateQuaility() {
        increaseQuality();
        if (itemHasExpired()) {
            increaseQuality();
        }
    }

    private void increaseQuality() {
        if (item.quality < 50) {
            item.quality += 1;
        }
    }

    private boolean itemHasExpired() {
        boolean condition;
        if (item.sellIn < 0) {
            condition = true;
        } else {
            condition = false;
        }
        return condition;
    }
}
