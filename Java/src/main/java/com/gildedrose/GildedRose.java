package com.gildedrose;

import com.gildedrose.item.Item;
import com.gildedrose.item.CustomisedItemFactory;
import com.gildedrose.item.QualityValues;

class GildedRose {

    private static final int LOWEST_QUALITY_VALUE_POSSIBLE = 0;
    private final CustomisedItemFactory customisedItemFactory;
    Item[] items;

    public GildedRose(Item[] items) {
        this.customisedItemFactory = new CustomisedItemFactory();
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            customisedItemFactory.customiseItem(item).updateState();
            if (hasReachedLowestQualityValue(item)) {
                item.quality = LOWEST_QUALITY_VALUE_POSSIBLE;
            } else if (hasReachedHighestQualityValue(item)) {
                item.quality = QualityValues.highestValuePossible(item);
            }
        }
    }

    private boolean hasReachedLowestQualityValue(Item item) {
        return item.quality < LOWEST_QUALITY_VALUE_POSSIBLE;
    }

    private boolean hasReachedHighestQualityValue(Item item) {
        return item.quality > QualityValues.highestValuePossible(item);
    }
}