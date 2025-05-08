package com.gildedrose.updaters;

import com.gildedrose.Item;

public class AgedBrieUpdater extends ItemUpdater {
    public AgedBrieUpdater(Item item) {
        super(item);
    }

    @Override
    public void update() {
        increaseQuality(1);
        decreaseSellIn();
        if (item.sellIn < 0) {
            increaseQuality(1);
        }
    }
}
