package com.gildedrose.items;

import com.gildedrose.Item;

public class LegendaryGildedRoseItem extends AbstractGildedRoseItem {

    public LegendaryGildedRoseItem(Item item) {
        super(item);

    }

    @Override
    public GildedRoseItem updateQuality() {
        return this;
    }
}
