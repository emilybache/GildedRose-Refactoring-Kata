package com.gildedrose;

import com.gildedrose.item.Item;
import com.gildedrose.item.CustomisedItemFactory;

class GildedRose {

    private final CustomisedItemFactory itemFactory;
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
        this.itemFactory = new CustomisedItemFactory();
    }

    public void updateQuality() {
        for (Item item : items) {
            itemFactory.customiseItem(item).updateState();
        }
    }
}