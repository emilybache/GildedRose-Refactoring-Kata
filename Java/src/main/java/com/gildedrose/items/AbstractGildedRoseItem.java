package com.gildedrose.items;

import com.gildedrose.Item;

public abstract class AbstractGildedRoseItem implements GildedRoseItem{

    protected final Item item;

    public AbstractGildedRoseItem(Item item) {
        this.item = item;
    }

    @Override
    public Item getItem() {
        return item;
    }
}
