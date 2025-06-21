package com.gildedrose;

public class NormalItem extends UpdatableItem {
    public NormalItem(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }
    @Override
    public void update() {
        sellIn--;
        if (quality > 0) quality--;
        if (sellIn < 0 && quality > 0) quality--;
    }
} 