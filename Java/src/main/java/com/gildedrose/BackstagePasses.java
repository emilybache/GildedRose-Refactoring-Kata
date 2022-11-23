package com.gildedrose;

public class BackstagePasses extends StorageItem {
    public BackstagePasses(Item item) {
        super(item);
    }

    @Override
    protected void updateQuality() {
        increaseQuality();

        if (item.sellIn < 11) {
            increaseQuality();
        }

        if (item.sellIn < 6) {
            increaseQuality();
        }
    }

    @Override
    protected void updateExpired() {
        item.quality = 0;
    }
}
