package com.gildedrose;

public class Item {

    private final String name;

    public int sellIn;

    public int quality;

    public Item(String name, int sellIn, int quality) {
        this.name = name;
        this.sellIn = sellIn;
        this.quality = quality;
    }

    public String getName() {
        return name;
    }

    public void setQualityToZero() {
        quality = 0;
    }

    public void deductSellIn() {
        sellIn--;
    }

    public void addOneToQuality() {
        quality++;
    }

    public void deductOneFromQuality() {
        quality--;
    }

   @Override
   public String toString() {
        return this.name + ", " + this.sellIn + ", " + this.quality;
    }
}
