package com.gildedrose.item;

public class Backstage extends Item {

    public static String BACKSTAGE = "Backstage passes to a TAFKAL80ETC concert";

    public Backstage(int sellIn, int quality) {
        super(BACKSTAGE, sellIn, quality);
    }

    @Override
    public void increaseQuality() {
        super.increaseQuality();
        if (this.sellIn < 11 && this.quality < 50) {
            super.increaseQuality();
        }
        if (this.sellIn < 6 && this.quality < 50) {
            super.increaseQuality();
        }
    }

    @Override
    public void updateQuality() {
        if (this.quality < 50) {
            this.increaseQuality();
        }
        if (this.sellIn < 0) {
            this.quality = 0;
        }
    }
}
