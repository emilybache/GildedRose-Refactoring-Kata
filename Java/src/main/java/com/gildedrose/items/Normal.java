package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.item_helpers.ItemType;

import static java.lang.Math.max;

public class Normal implements ItemType {

    private final Item item;

    public Normal(Item item) {
        this.item = item;
    }

    @Override
    public void updateQuality() {
        decrementSellInDate();
        if (qualityIsPositive()) {
            if (sellInDatePasses()) {
                decrementQualityByTwo();
            } else {
                decrementQuality();
            }
        }
    }

    private boolean qualityIsPositive() {
        return item.quality > 0;
    }

    private void decrementSellInDate() {
        this.item.sellIn--;
    }

    private void decrementQualityByTwo() {
        this.item.quality = max(this.item.quality - 2, 0);
    }

    private boolean sellInDatePasses() {
        return this.item.sellIn < 0;
    }

    private void decrementQuality() {
        this.item.quality--;
    }
}
