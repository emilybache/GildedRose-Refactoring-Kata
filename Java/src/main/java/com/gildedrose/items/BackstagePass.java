package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.item_helpers.ItemType;

public class BackstagePass implements ItemType {

    private final Item item;

    public BackstagePass(Item item) {
        this.item = item;
    }

    @Override
    public void updateQuality() {
        decrementSellInDate();
        determineQuality();
    }

    private void determineQuality() {
        if (this.item.sellIn >= 10) {
            incrementQuality();
        } else if (this.item.sellIn >= 5) {
            incrementQualityByTwo();
        } else if (this.item.sellIn >= 0) {
            incrementQualityByThree();
        } else {
            makeQualityZero();
        }
    }

    private void decrementSellInDate() {
        this.item.sellIn--;
    }

    private void incrementQuality() {
        this.item.quality++;
    }

    private void makeQualityZero() {
        this.item.quality = 0;
    }

    private void incrementQualityByTwo() {
        this.item.quality = this.item.quality + 2;
    }

    private void incrementQualityByThree() {
        this.item.quality = this.item.quality + 3;
    }

}
