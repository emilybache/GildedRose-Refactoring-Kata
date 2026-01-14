package com.gildedrose;

public class Item {

    public final static String AGED_BRIE = "Aged Brie";
    public final static String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    public final static String SULFURAS = "Sulfuras, Hand of Ragnaros";
    public String name;

    public int sellIn;

    public int quality;

    public Item(String name, int sellIn, int quality) {
        this.name = name;
        this.sellIn = sellIn;
        this.quality = quality;
    }

    public boolean isBackstagePasses() {
        return name.equals(BACKSTAGE_PASSES);
    }

    public boolean isAgedBride() {
        return name.equals(AGED_BRIE);
    }

    @Override
   public String toString() {
        return this.name + ", " + this.sellIn + ", " + this.quality;
    }

    void decreaseSellInEachDay() {
        if (isNotSulfuras()) {
            sellIn--;
        }
    }

    private boolean isNotSulfuras() {
        return !name.equals(SULFURAS);
    }

    void increaseQualityByOne() {
        if (quality < 50) {
            quality++;
        }
    }

    void increaseQualityByTwo() {
        increaseQualityByOne();
        increaseQualityByOne();
    }

    void increaseQualityByThree() {
        increaseQualityByTwo();
        increaseQualityByOne();
    }

    void decreaseQualityByOne() {
        if (quality > 0) {
            if (isNotSulfuras()) {
                quality--;
            }
        }
    }

    void increaseQualityBackstage() {
        if (sellIn < 6) {
            increaseQualityByThree();
        } else if (sellIn < 11) {
            increaseQualityByTwo();
        } else {
            increaseQualityByOne();
        }
    }
}
