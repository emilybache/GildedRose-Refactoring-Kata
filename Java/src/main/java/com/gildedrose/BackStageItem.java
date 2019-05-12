package com.gildedrose;

public class BackStageItem {
    private Item item;

    public BackStageItem(Item item) {
        this.item=item;
    }

    public void update() {
        increaseQuality();
        if (item.sellIn < 10) {
            increaseQuality();
        }
        if (item.sellIn < 5) {
            increaseQuality();
        }
        if (itemHasExpired()) {
            item.quality -= item.quality;
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
