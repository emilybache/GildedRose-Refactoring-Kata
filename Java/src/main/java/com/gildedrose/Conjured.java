package com.gildedrose;

public class Conjured extends InventoryItem {
    public Conjured(Item item) {
        super(item);
    }

    @Override
    void age() {
        decreaseQuality();
        decreaseSellIn();
        if (item.sellIn < Constants.SELLIN_DAY) decreaseQuality();
    }

    @Override
    protected void decreaseQuality() {
        if (item.quality > Constants.MIN_QUALITY) item.quality -= 2;
    }
}
