package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.item_helpers.ItemType;

import static java.lang.Math.max;

public class Conjured implements ItemType {

    private final Item item;

    public Conjured(Item item) {
        this.item = item;
    }

    @Override
    public void updateQuality() {
        decrementSellInDate();
        if (qualityIsGreaterThanZero()) {
            if (sellInDatePasses()) {
                decrementQualityByFour();
            } else {
                decrementQuality();
            }
        }
    }

    private boolean qualityIsGreaterThanZero() {
        return item.quality > 0;
    }

    private void decrementSellInDate() {
        this.item.sellIn--;
    }

    private void decrementQualityByFour() {
        this.item.quality = max(this.item.quality - 4, 0);
    }

    private boolean sellInDatePasses() {
        return this.item.sellIn < 0;
    }

    private void decrementQuality() {
        this.item.quality--;
    }
}
