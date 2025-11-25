package com.gildedrose.items;

import com.gildedrose.Item;

public class AgedBrieItem extends GildedItem {
    public AgedBrieItem(Item item) { super(item); }

    @Override
    public void updateQuality() {
        increaseQuality(1);
        decreaseSellIn();
        if (item.sellIn < 0)
            increaseQuality(1);
    }
}

