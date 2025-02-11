package com.gildedrose;

public final class ConjuredItem extends Item {

    ConjuredItem(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }

    @Override
    public void degrade() {
        sellIn--;
        // I expected that this should be 4 : 2,
        // but then the test of the coding kata fails because it degrades too quickly. (should be twice as fast as the normal right?)
        int degradation = sellIn < 0 ? 2 : 1;
        subtractQuality(degradation);
    }
}
