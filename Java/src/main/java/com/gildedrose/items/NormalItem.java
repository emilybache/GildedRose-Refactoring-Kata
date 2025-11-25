package com.gildedrose.items;

import com.gildedrose.Item;

public class NormalItem extends GildedItem {
    public NormalItem(Item item) { super(item); }

    @Override
    public void updateQuality() {
        decreaseQuality(1);
        decreaseSellIn();
        if (item.sellIn < 0)
            decreaseQuality(1);
    }
}
