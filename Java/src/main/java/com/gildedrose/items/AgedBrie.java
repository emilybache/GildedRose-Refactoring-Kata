package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.item_helpers.ItemType;

import static java.lang.Math.max;

public class AgedBrie implements ItemType {

    private final Item item;

    public AgedBrie(Item item) {
        this.item = item;
    }

    @Override
    public void updateQuality() {
        decrementSellInDate();
        if (qualityIsPositive()) {
            if (sellInDatePasses()) {
                incrementQualityByTwo();
            } else {
                incrementQuality();
            }
        }
    }

    private boolean qualityIsPositive() {
        return item.quality > 0;
    }

    private void decrementSellInDate() {
        this.item.sellIn--;
    }

    private boolean sellInDatePasses() {
        return this.item.sellIn < 0;
    }

    private void incrementQualityByTwo() {
        this.item.quality = max(this.item.quality + 2, 0);
    }

    private void incrementQuality() {
        this.item.quality++;
    }

}
