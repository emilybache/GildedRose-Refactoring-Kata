package com.gildedrose.item;

public class Sulfuras extends Item {

    public static String SULFURAS = "Sulfuras, Hand of Ragnaros";

    public Sulfuras(int sellIn, int quality) {
        super(SULFURAS, sellIn, quality);
    }

    @Override
    public void updateSellIn() {
        //NO-OP
    }

    @Override
    public void updateQuality() {
        //NO-OP
    }
}
