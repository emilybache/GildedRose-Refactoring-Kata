package com.gildedrose;

public class Backstage extends InventoryItem {

    public Backstage(Item item) {
        super(item);
    }

    @Override
    void age() {
        if (item.quality < Constants.MAX_QUALITY) {
            item.quality = item.quality + 1;
            if (item.sellIn < 11 && item.quality < Constants.MAX_QUALITY) item.quality++;
            if (item.sellIn < 6 && item.quality < Constants.MAX_QUALITY) item.quality++;
        }
        decreaseSellIn();
        if (item.sellIn < Constants.SELLIN_DAY) item.quality = Constants.MIN_QUALITY;
    }
}
