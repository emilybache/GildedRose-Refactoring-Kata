package com.gildedrose;

import static com.gildedrose.Constants.MAX_QUALITY;

public class AgedBrie extends InventoryItem {
    public AgedBrie(Item item) {
        super(item);
    }

    @Override
    void age() {
        if (item.quality < MAX_QUALITY) {
            item.quality++;
        }
        decreaseSellIn();
        if (item.sellIn < Constants.SELLIN_DAY && item.quality < MAX_QUALITY) item.quality++;
    }
}
