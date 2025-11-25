package com.gildedrose.items;

import com.gildedrose.Item;

public class BackstageItem extends GildedItem {
    public BackstageItem(Item item) { super(item); }

    @Override
    public void updateQuality() {
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

