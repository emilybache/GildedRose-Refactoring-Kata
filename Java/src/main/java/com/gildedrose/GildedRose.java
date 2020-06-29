package com.gildedrose;

import com.gildedrose.item.GRItem;
import com.gildedrose.item.GRItemFactory;

import java.util.Arrays;

class GildedRose {

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        Arrays.stream(items).map(GRItemFactory::create).forEach(GRItem::updateQuality);
    }
}