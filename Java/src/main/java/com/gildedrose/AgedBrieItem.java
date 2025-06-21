package com.gildedrose;

public class AgedBrieItem extends UpdatableItem {
    public AgedBrieItem(int sellIn, int quality) {
        super("Aged Brie", sellIn, quality);
    }
    @Override
    public void update() {
        sellIn--;
        if (quality < 50) quality++;
        if (sellIn < 0 && quality < 50) quality++;
    }
} 