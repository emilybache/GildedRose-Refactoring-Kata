package com.gildedrose.updaters;

import com.gildedrose.Item;

public class BackstagePassUpdater extends ItemUpdater {
    public BackstagePassUpdater(Item item) {
        super(item);
    }

    @Override
    public void update() {
        if (item.sellIn <= 0) {
            item.quality = 0;
        } else if (item.sellIn <= 5) {
            increaseQuality(3);
        } else if (item.sellIn <= 10) {
            increaseQuality(2);
        } else {
            increaseQuality(1);
        }
        decreaseSellIn();
    }
}
