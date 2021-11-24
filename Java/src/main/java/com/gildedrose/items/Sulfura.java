package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.item_helpers.ItemType;

public class Sulfura implements ItemType {

    private static final int QUALITY = 80;
    private final Item item;

    public Sulfura(Item item) {
        this.item = item;
    }

    @Override
    public void updateQuality() {
        decrementSellInDate();
        setQualityTo80();
    }

    private void setQualityTo80() {
        if (this.item.quality != QUALITY) {
            this.item.quality = QUALITY;
        }
    }

    private void decrementSellInDate() {
        this.item.sellIn--;
    }
}
