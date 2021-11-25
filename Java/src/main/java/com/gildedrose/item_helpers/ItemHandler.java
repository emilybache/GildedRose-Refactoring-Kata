package com.gildedrose.item_helpers;

import com.gildedrose.Item;

import static java.lang.Math.max;

public class ItemHandler {

    private static final int LEGENDARY_ITEM_QUALITY = 80;

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

    public void incrementQualityByTwo() {
        item.quality = max(item.quality + 2, 0);
    }

    public void incrementQuality() {
        item.quality++;
    }

    public void decrementQuality() {
        item.quality--;
    }

    public void decrementQualityBy4() {
        item.quality = max(item.quality - 4, 0);
    }

    public void decrementQualityBy2() {
        item.quality = max(item.quality - 2, 0);
    }

    public void setLegendaryQuality() {
        if (item.quality != LEGENDARY_ITEM_QUALITY) {
            item.quality = LEGENDARY_ITEM_QUALITY;
        }
    }

    public boolean sellInLessThan5Days() {
        return item.sellIn >= 0 && item.sellIn <= 5;
    }

    public boolean sellInLessThan10Days() {
        return item.sellIn >= 5 && item.sellIn <= 10;
    }

    public boolean sellInDaysMoreThan10Days() {
        return item.sellIn >= 10;
    }

    public boolean qualityIsHigherThanZero() {
        return item.quality > 0;
    }

    public void makeQualityZero() {
        item.quality = 0;
    }

    public void incrementQualityBy3() {
        item.quality = max(item.quality + 3, 0);
    }

}
