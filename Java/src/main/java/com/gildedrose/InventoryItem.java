package com.gildedrose;

import static com.gildedrose.Constants.*;

public class InventoryItem {
    protected final Item item;

    InventoryItem(Item item) {
        this.item = item;
    }

    public static InventoryItem createInventoryItem(Item item) {
        if (item.name.equals(SULFURAS)) return new Sulfuras(item);
        if (item.name.equals(AGED_BRIE)) return new AgedBrie(item);
        if (item.name.equals(BACKSTAGE)) return new Backstage(item);
        if (item.name.equals(CONJURED)) return new Conjured(item);
        return new InventoryItem(item);
    }

    void age() {
        decreaseQuality();
        decreaseSellIn();
        if (item.sellIn < Constants.SELLIN_DAY) decreaseQuality();
    }

    protected void decreaseSellIn() {
        item.sellIn--;
    }

    protected void decreaseQuality() {
        if (item.quality > Constants.MIN_QUALITY) item.quality--;
    }
}
