package com.gildedrose;

import com.gildedrose.items.GildedRoseItem;
import com.gildedrose.items.GildedRoseItemFactory;

import java.util.Arrays;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        items = Arrays.stream(items)
            .map(GildedRoseItemFactory::create)
            .map(GildedRoseItem::updateQuality)
            .map(GildedRoseItem::getItem)
            .toArray(Item[]::new);
    }

}
