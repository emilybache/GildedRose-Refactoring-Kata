package com.gildedrose;

import java.util.Objects;

import static java.util.Objects.hash;

// I think ideally I should do this with a sealed interface instead, but for the current available types of items
// a sealed class seemed more convenient to me (especially because of shared member variables and setter methods).
public sealed abstract class Item permits
    AgedBrieItem,
    BackstagePassItem,
    NormalItem,
    SulfurasItem {

    private final String name;

    public int sellIn;

    public int quality;

    Item(String name, int sellIn, int quality) {
        this.name = name;
        this.sellIn = sellIn;
        this.quality = quality;
    }

    public abstract void degrade();


    public String getName() {
        return name;
    }

    protected void addQuality(int addition) {
        if ((quality + addition) <= 50) {
            quality += addition;
        }
    }

    protected void subtractQuality(int subtraction) {
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
        var item = (Item) o;
        return sellIn == item.sellIn && quality == item.quality && Objects.equals(name, item.name);
    }

    @Override
    public int hashCode() {
        return hash(name, sellIn, quality);
    }
}
