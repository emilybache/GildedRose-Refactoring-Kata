package com.gildedrose;

import java.util.List;

import static com.gildedrose.item_helpers.ItemFactory.getItem;
import static java.util.Collections.singletonList;

class GildedRose {
    List<Item> items;

    public GildedRose(List<Item> items) {
        this.items = items;
    }

    public GildedRose(Item items) {
        this.items = singletonList(items);
    }

    public void updateQuality() {
        items.forEach(item -> getItem(item).updateQuality());
    }
}
