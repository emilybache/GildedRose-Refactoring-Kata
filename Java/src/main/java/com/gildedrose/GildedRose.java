package com.gildedrose;

import com.gildedrose.experts.ItemUpdaterFactory;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            ItemUpdater expert = new ItemUpdaterFactory().createItemUpdater(item);
            expert.update(item);
        }
    }
}
