package com.gildedrose;

public class AgeddBrie extends StorageItem {
    public static final String NAME = "Aged Brie";

    public AgeddBrie(Item item) {
        super(item);
    }

    @Override
    protected void updateQuality() {
        increaseQuality();
    }

    @Override
    protected void updateExpired() {
        increaseQuality();
    }
}
