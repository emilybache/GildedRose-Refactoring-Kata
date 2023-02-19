package com.gildedrose;

public class Item {

    private String name;

    private int sellIn;

    private int quality;

    public Item(String name, int sellIn, int quality) {
        this.name = name;
        this.sellIn = sellIn;
        this.quality = quality;
    }

    public String getName() {
        return this.name;
    }

    public int getSellIn() {
        return this.sellIn;
    }

    public void setSellIn(int value) {
        this.sellIn = value;
    }

    public int getQuality() {
        return this.quality;
    }

    public void setQuality(int value) {
        this.quality = value;
    }

    @Override
    public String toString() {
        return this.name + ", " + this.sellIn + ", " + this.quality;
    }
}
