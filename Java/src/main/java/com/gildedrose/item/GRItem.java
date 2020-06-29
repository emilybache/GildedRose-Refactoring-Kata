package com.gildedrose.item;

import com.gildedrose.Item;

import static com.gildedrose.item.GRItemFactory.SULFURAS;

public abstract class GRItem {

    protected static final int MAX_QUALITY = 50;
    protected static final int MIN_QUALITY = 0;

    protected final Item item;

    GRItem(Item item) {
        validateItem(item);
        this.item = item;
    }

    public abstract void updateQuality();

    private static void validateItem(Item item) {
        if (item.quality < GRItem.MIN_QUALITY) {
            throw new ItemQualityIsNegativeException(item.name);
        } else if (item.quality > GRItem.MAX_QUALITY && !item.name.equals(SULFURAS)) {
            throw new ItemQualityExceedsMaxValueException(item.name);
        }
    }
}
