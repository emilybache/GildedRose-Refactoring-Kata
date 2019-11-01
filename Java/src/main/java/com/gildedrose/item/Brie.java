package com.gildedrose.item;

public class Brie extends Item {

    public static String BRIE = "Aged Brie";

    public Brie(int sellIn, int quality) {
        super(BRIE, sellIn, quality);
    }

    @Override
    public void increaseQuality(){
        if (this.quality < 50) {
            super.increaseQuality();
        }
    }
}
