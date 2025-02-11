package com.gildedrose;

public final class NormalItem extends Item {

    NormalItem(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }

    @Override
    public void degrade() {
        sellIn--;
        int degradation = sellIn < 0 ? 2 : 1;
        subtractQuality(degradation);

    }
}
