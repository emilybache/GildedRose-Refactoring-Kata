package com.gildedrose.item_helpers;

import com.gildedrose.Item;

import static java.lang.Math.max;

public class ItemHandler {

    private static final int QUALITY = 80;

    private final Item item;

    public ItemHandler(Item item) {
        this.item = item;
    }

    public void decrementSellInDate() {
        this.item.sellIn--;
    }

    public boolean qualityIsHigherThanZero() {
        return item.quality > 0;
    }

    public boolean sellInDatePasses() {
        return this.item.sellIn < 0;
    }

    public void incrementQualityByTwo() {
        this.item.quality = max(this.item.quality + 2, 0);
    }

    public void incrementQuality() {
        this.item.quality++;
    }

    public void decrementQuality() {
        this.item.quality--;
    }

    public void decrementQualityBy4() {
        this.item.quality = max(this.item.quality - 4, 0);
    }

    public void decrementQualityBy2() {
        this.item.quality = this.item.quality - 2;
    }

    public void setQualityTo80() {
        if (this.item.quality != QUALITY) {
            this.item.quality = QUALITY;
        }
    }

    public boolean sellInLessThan5Days() {
        return this.item.sellIn >= 0 && this.item.sellIn <= 5;
    }

    public boolean sellInLessThan10Days() {
        return this.item.sellIn >= 5 && this.item.sellIn <= 10;
    }

    public boolean sellInDaysMoreThan10Days() {
        return this.item.sellIn >= 10;
    }

    public void makeQualityZero() {
        this.item.quality = 0;
    }

    public void incrementQualityBy3() {
        this.item.quality = this.item.quality + 3;
    }

}
