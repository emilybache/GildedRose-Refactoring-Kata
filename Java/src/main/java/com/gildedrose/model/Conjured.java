package com.gildedrose.model;

import static com.gildedrose.Constants.MIN_QUALITY;

public class Conjured extends Goods {

    public Conjured(int sellIn, int quality) {
        super("Conjured Mana Cake", sellIn, quality);
    }

    @Override
    public void updateQuality() {
        if (sellIn > 0) {
            degradeQualityBy(2);
        } else {
            degradeQualityBy(4);
        }
        sellInPasses();
    }

    private void degradeQualityBy(int degradeBy) {
        quality = Math.max(quality - degradeBy, MIN_QUALITY);
    }
}
