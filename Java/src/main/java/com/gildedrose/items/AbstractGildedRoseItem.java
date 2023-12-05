package com.gildedrose.items;

import com.gildedrose.Item;

public abstract class AbstractGildedRoseItem implements GildedRoseItem{

    protected final static int MAX_QUALITY = 50;

    protected final Item item;

    public AbstractGildedRoseItem(Item item) {
        this.item = item;
    }

    @Override
    public Item getItem() {
        return item;
    }

    protected int getSellIn() {
        return getItem().sellIn;
    }

    protected int getQuality() {
        return getItem().quality;
    }
}
