package com.gildedrose;

public class AgeddBrie extends StorageItem {
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
