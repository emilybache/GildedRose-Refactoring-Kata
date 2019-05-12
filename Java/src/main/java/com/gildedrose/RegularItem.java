package com.gildedrose;

public class RegularItem {
    private Item item;

    public RegularItem(Item item) {
        this.item = item;
    }

    public void updateQuality() {
        if (itemHasExpired(item)) {
            decreaseQualityTwice();
        } else {
            decreaseQuality();
        }
    }

    private boolean itemHasExpired(Item item) {
        boolean condition;
        if (item.sellIn < 0) {
            condition = true;
        } else {
            condition = false;
        }
        return condition;
    }


    private void decreaseQualityTwice() {
        decreaseQuality();
        decreaseQuality();
    }

    private void decreaseQuality() {
        if (item.quality > 0) {
            item.quality -= 1;
        }
    }

}
