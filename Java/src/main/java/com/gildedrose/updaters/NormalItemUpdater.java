package com.gildedrose.updaters;

import com.gildedrose.Item;

public class NormalItemUpdater extends ItemUpdater {
    public NormalItemUpdater(Item item) {
        super(item);
    }

    @Override
    public void update() {
        decreaseQuality(1);
        decreaseSellIn();
        if (item.sellIn < 0) {
            decreaseQuality(1);
        }
    }
}
