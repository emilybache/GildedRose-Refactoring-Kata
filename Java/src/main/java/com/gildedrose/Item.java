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

    public void updateItem() {
        boolean isAgedBrie = name.equals("Aged Brie");
        boolean isSulfuras = name.equals("Sulfuras, Hand of Ragnaros");
        boolean isBackStagePass = name.equals("Backstage passes to a TAFKAL80ETC concert");

        if (!isAgedBrie && !isBackStagePass) {
            if (quality > 0 && !isSulfuras) {
                deductOneFromQuality();
            }
        } else if(quality < 50) {
            addOneToQuality();
            if (isBackStagePass) {
                if (sellIn < 11 && quality < 50) {
                    addOneToQuality();
                }

                if (sellIn < 6 && quality < 50) {
                    addOneToQuality();
                }
            }
        }

        if (!isSulfuras) {
            deductSellIn();
        }

        if (sellIn < 0) {
            if (!isAgedBrie) {
                if (!isBackStagePass) {
                    if (quality > 0 && !isSulfuras) {
                        deductOneFromQuality();
                    }
                } else {
                    setQualityToZero();
                }
            } else if ((quality < 50)) {
                addOneToQuality();
            }
        }
    }

    private void setQualityToZero() {
        quality = 0;
    }

    private void deductSellIn() {
        sellIn--;
    }

    private void addOneToQuality() {
        quality++;
    }

    private void deductOneFromQuality() {
        quality--;
    }

   @Override
   public String toString() {
        return this.name + ", " + this.sellIn + ", " + this.quality;
    }
}
