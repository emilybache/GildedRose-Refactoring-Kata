package com.gildedrose.item;

final class QualityValues {

    static int lowestValuePossible() {
        return 0;
    }

    static int highestValuePossible(Item item) {
        if (item.name.equals(CustomisedItemFactory.SULFURAS)) {
            return 80;
        }
        return 50;
    }
}
