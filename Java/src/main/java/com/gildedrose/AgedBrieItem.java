package com.gildedrose;

public final class AgedBrieItem extends Item {

    AgedBrieItem(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }

    @Override
    public void degrade() {
        addQuality(1);

        sellIn--;

        if (sellIn < 0) {
            addQuality(1);
        }
    }
}
