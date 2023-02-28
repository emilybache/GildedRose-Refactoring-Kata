package com.gildedrose.model;

public class Sulfuras extends Goods {

    public Sulfuras(int sellIn, int quality) {
        super("Sulfuras, Hand of Ragnaros", sellIn, quality);
    }

    @Override
    public void updateQuality() {
        // do  nothing
    }
}
