package com.gildedrose.item_helpers;

import com.gildedrose.main.Item;

import static java.lang.Math.max;

public class ItemHandler {

    private final Item item;

    public ItemHandler(Item item) {
        this.item = item;
    }

    public void decrementSellInDate() {
        item.sellIn--;
    }

    public boolean beforeSellInDate() {
        return item.sellIn >= 0;
    }

    public boolean lessThan5DaysToSellIn() {
        return item.sellIn >= 0 && item.sellIn <= 5;
    }

    public boolean lessThan10DaysToSellIn() {
        return item.sellIn >= 5 && item.sellIn <= 10;
    }

    public boolean moreThan10DaysToSellIn() {
        return item.sellIn >= 10;
    }

    public void makeQualityZero() {
        item.quality = 0;
    }

    public void incrementQuality() {
        item.quality = max(item.quality + 1, 0);
    }

    public void incrementQualityBy2() {
        item.quality = max(item.quality + 2, 0);
    }

    public void incrementQualityBy3() {
        item.quality = max(item.quality + 3, 0);
    }

    public void decrementQuality() {
        item.quality = max(item.quality - 1, 0);
    }

    public void decrementQualityBy2() {
        item.quality = max(item.quality - 2, 0);
    }

    public void decrementQualityBy4() {
        item.quality = max(item.quality - 4, 0);
    }

}
