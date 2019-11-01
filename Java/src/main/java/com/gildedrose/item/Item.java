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

   @Override
   public String toString() {
        return this.name + ", " + this.sellIn + ", " + this.quality;
    }

    public void decreaseQuality() {
        this.quality = this.quality - 1;
    }

    public void increaseQuality() {
        this.quality = this.quality + 1;
    }

    public void updateSellIn() {
        this.sellIn = this.sellIn - 1;
    }
}
