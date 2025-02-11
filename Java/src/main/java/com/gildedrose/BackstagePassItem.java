package com.gildedrose;

public final class BackstagePassItem extends Item {

    BackstagePassItem(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }

    @Override
    public void degrade() {
        addQuality(1);

        if (sellIn < 11) {
            addQuality(1);
        }

        if (sellIn < 6) {
            addQuality(1);
        }

        sellIn--;

        if (sellIn < 0) {
            quality = 0;
        }
    }
}
