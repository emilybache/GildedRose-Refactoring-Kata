package com.gildedrose.updaters;

import com.gildedrose.Item;

public class ConjuredItemUpdater extends ItemUpdater {
    public ConjuredItemUpdater(Item item) {
        super(item);
    }

    @Override
    public void update() {
        decreaseQuality(2);
        decreaseSellIn();
        if (item.sellIn < 0) {
            decreaseQuality(2);
        }
    }
}
