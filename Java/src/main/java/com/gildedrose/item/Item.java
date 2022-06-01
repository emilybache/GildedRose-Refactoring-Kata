/* (C)2022 */
package com.gildedrose.item;

public class Item {

    public String name;

    public int sellIn;

    public int quality;

    public Item(String name, int sellIn, int quality) {
        this.name = name;
        this.sellIn = sellIn;
        this.quality = quality;
    }

    public void updateQuality() {
        if (sellIn <= 0) {
            quality -= 2;
        } else {
            quality -= 1;
        }
        if (quality < 0) {
            quality = 0;
        }
    }

    public void updateSellIn() {
        sellIn -= 1;
    }

    @Override
    public String toString() {
        return this.name + ", " + this.sellIn + ", " + this.quality;
    }
}
