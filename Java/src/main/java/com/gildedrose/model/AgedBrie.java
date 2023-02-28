package com.gildedrose.model;

import static com.gildedrose.Constants.MAX_QUALITY;

public class AgedBrie extends Goods {

    public static final String AGED_BRIE = "Aged Brie";

    public AgedBrie(int sellIn, int quality) {
        super(AGED_BRIE, sellIn, quality);
    }

    @Override
    public void updateQuality() {

        sellInPasses();
        upgradeQuality();
        if (sellIn < 0) {
            upgradeQuality();
        }

    }

    private void upgradeQuality() {
        quality = Math.min(quality + 1, MAX_QUALITY);
    }

}
