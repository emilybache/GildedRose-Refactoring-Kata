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
        return new InventoryItem(item);
    }

    void age() {
        decreaseQuality();
        decreaseSellIn();
        if (item.sellIn < 0) decreaseQuality();
    }

    protected void decreaseSellIn() {
        item.sellIn = item.sellIn - 1;
    }

    protected void decreaseQuality() {
        if (item.quality > 0) item.quality = item.quality - 1;
    }
}
