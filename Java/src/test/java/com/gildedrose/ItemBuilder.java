package com.gildedrose;

public class ItemBuilder {
    private String name;
    private int sellIn;
    private int quality;

    public ItemBuilder setName(String name) {
        this.name = name;
        return this;
    }

    public ItemBuilder setSellIn(int sellIn) {
        this.sellIn = sellIn;
        return this;
    }

    public ItemBuilder setQuality(int quality) {
        this.quality = quality;
        return this;
    }

    public Item createItem() {
        return new Item(name, sellIn, quality);
    }
}
