/* (C)2022 */
package com.gildedrose.item;

public class Sulfuras extends Item {
    public static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    public static final int QUALITY = 80;

    public Sulfuras(int sellIn) {
        super(SULFURAS, sellIn, QUALITY);
    }

    @Override
    public void updateQuality() {}

    @Override
    public void updateSellIn() {}
}
