package com.gildedrose.item;

public class AgedBrie extends Item {

    public AgedBrie(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }

    public void updateYourState() {
        sellIn -= 1;
        quality += 1;
    }
}
