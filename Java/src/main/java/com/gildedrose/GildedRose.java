package com.gildedrose;

import java.util.List;

class GildedRose {
    private final List<Item> items;

    public GildedRose(Item[] items) {
        this.items = List.of(items);
    }

    public void degradeItems() {
        items.forEach(Item::degrade);
    }
}
