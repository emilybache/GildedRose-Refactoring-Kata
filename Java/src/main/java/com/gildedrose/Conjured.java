package com.gildedrose;

public class Conjured extends StorageItem {

    public static final String NAME = "Conjured";
    public Conjured(Item item) {
        super(item);
    }

    @Override
    protected void decreaseQuality() {
        item.quality = Math.max(0,item.quality-2);
    }
}
