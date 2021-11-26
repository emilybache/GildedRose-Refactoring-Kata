package com.gildedrose.main;

import java.util.List;

import static com.gildedrose.item_helpers.ItemFactory.getItemType;
import static java.util.Collections.singletonList;

public class GildedRose {
    private final List<Item> items;

    public GildedRose(List<Item> items) {
        this.items = items;
    }

    public GildedRose(Item items) {
        this.items = singletonList(items);
    }

    public void updateQuality() {
        items.forEach(item -> getItemType(item).updateQuality());
    }
}
