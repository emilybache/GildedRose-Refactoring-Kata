package com.gildedrose.service;

import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Setter
public abstract class InventoryItem {
    private static final int MAX_QUALITY = 50;

    public String name;
    public int quality;
    public int sellIn;

    public abstract boolean qualityDecreaseInverted();

    public abstract int qualityDecreaseAmount();

    public abstract int handleQualityAfterSellIn();

    public int decreaseQualityAboveZero() {
        quality = quality > 0 ? quality - 1 : 0;
        return quality;
    }

    public int increaseQualityBelowMaximum() {
        if (quality < MAX_QUALITY) {
            quality++;
        }
        return quality;
    }

    public int reduceSellIn() {
        sellIn--;
        return sellIn;
    }
}
