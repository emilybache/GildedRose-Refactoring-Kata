package com.gildedrose;

import java.util.Arrays;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        Arrays.stream(items).forEach(GRItem::validateItem);
        this.items = items;
    }

    public void updateQuality() {
        Arrays.stream(items).map(GRItem::new).forEach(GRItem::updateQuality);
    }
}