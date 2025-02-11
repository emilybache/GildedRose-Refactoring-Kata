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
            case AgedBrie -> updateAgedBrieItem();
            case BackstagePass -> updateBackstagePassItem();
            case Sulfuras -> {}
            case Normal -> updateNormalItem();
        }
    }

    private void updateNormalItem() {
        sellIn--;

        subtractQuality(1);

        if (sellIn < 0) {
            subtractQuality(1);
        }
    }

    private void updateBackstagePassItem() {
        addQuality(1);

        if (sellIn < 11) {
            addQuality(1);
        }

        if (sellIn < 6) {
            addQuality(1);
        }

        sellIn--;

        if (sellIn < 0) {
            quality = 0;
        }
    }

    private void updateAgedBrieItem() {
        addQuality(1);

        sellIn--;

        if (sellIn < 0) {
            addQuality(1);
        }
    }

    private void addQuality(int addition) {
        if ((quality + addition) <= 50) {
            quality += addition;
        }
    }

    private void subtractQuality(int subtraction) {
        if ((quality - subtraction) >= 0) {
            quality -= subtraction;
        }
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
