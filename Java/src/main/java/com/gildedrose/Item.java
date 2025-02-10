package com.gildedrose;

import java.util.Objects;

import static com.gildedrose.ItemType.fromName;

public class Item {

    private final String name;

    private final ItemType type;

    public int sellIn;

    public int quality;

    public Item(String name, int sellIn, int quality) {
        this.name = name;
        this.sellIn = sellIn;
        this.quality = quality;
        this.type = fromName(name);
    }

    public String getName() {
        return name;
    }

    public void updateItem() {
        switch (type) {
            case AgedBrie -> {

                if (quality < 50) {
                    addOneToQuality();
                }

                deductSellIn();

                if (sellIn < 0 && quality < 50) {
                    addOneToQuality();
                }
            }

            case BackstagePass -> {

                if (quality < 50) {
                    addOneToQuality();
                }

                if (sellIn < 11 && quality < 50) {
                    addOneToQuality();
                }

                if (sellIn < 6 && quality < 50) {
                    addOneToQuality();
                }
                deductSellIn();


                if (sellIn < 0) {
                    quality = 0;
                }
            }

            case Sulfuras -> {}

            case Unknown -> {
                deductSellIn();

                if (quality > 0) {
                    deductOneFromQuality();
                }

                if (sellIn < 0 && quality > 0) {
                    deductOneFromQuality();
                }
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

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        Item item = (Item) o;
        return sellIn == item.sellIn && quality == item.quality && Objects.equals(name, item.name) && type == item.type;
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, type, sellIn, quality);
    }
}
