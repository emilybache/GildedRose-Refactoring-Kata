package com.gildedrose;

public class SulfurasItem extends UpdatableItem {
    public SulfurasItem(int sellIn, int quality) {
        super("Sulfuras, Hand of Ragnaros", sellIn, quality);
    }
    @Override
    public void update() {
        // Legendary item, does not change
    }
} 