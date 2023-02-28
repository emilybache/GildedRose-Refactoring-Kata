package com.gildedrose.model;

import com.gildedrose.Item;

public abstract class Goods extends Item {
    public Goods(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }

    public abstract void updateQuality();

    public void sellInPasses() {
        sellIn = sellIn - 1;
    }
}
