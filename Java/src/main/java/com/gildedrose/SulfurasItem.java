package com.gildedrose;

public final class SulfurasItem extends Item {

    SulfurasItem(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }

    @Override
    public void degrade() {
        // do nothing
    }
}
