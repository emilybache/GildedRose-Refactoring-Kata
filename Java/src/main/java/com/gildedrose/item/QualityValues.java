package com.gildedrose.item;

public final class QualityValues {

    public static int highestValuePossible(Item item) {
        if (item.name.equals(CustomisedItemFactory.SULFURAS)) {
            return 80;
        }
        return 50;
    }
}
