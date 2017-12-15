package com.gildedrose.item;

public class StandardItem extends Item {

    public StandardItem(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }

    public void updateYourState() {
        sellIn -= 1;
        if (sellIn > 0) {
            quality -= 1;
        } else {
            quality -= 2;
        }
    }
}
