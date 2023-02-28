package com.gildedrose.model;

import static com.gildedrose.Constants.MIN_QUALITY;

public class NormalItem extends Goods {

    public NormalItem(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }

    @Override
    public void updateQuality() {
        sellInPasses();
        degradeQuality();
        if (sellIn < 0) {
            degradeQuality();
        }
    }

    private void degradeQuality() {
        quality = Math.max(quality - 1, MIN_QUALITY);
    }
}
