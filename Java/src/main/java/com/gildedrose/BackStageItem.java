package com.gildedrose;

public class BackStageItem extends RegularItem{
    public BackStageItem(Item item) {
        this.item=item;
    }

    public void update() {
        increaseQualityBy(1);
        if (item.sellIn < 10) {
            increaseQualityBy(1);
        }
        if (item.sellIn < 5) {
            increaseQualityBy(1);
        }
        if (itemHasExpired()) {
            item.quality -= item.quality;
        }
    }
}
