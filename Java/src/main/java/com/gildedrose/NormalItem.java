package com.gildedrose;

public final class NormalItem extends Item {

    NormalItem(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }

    @Override
    public void degrade() {
        sellIn--;

        subtractQuality(1);

        if (sellIn < 0) {
            subtractQuality(1);
        }
    }
}
