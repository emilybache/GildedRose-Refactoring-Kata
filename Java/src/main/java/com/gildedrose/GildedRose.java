package com.gildedrose;

import com.gildedrose.item.CustomisedItem;
import com.gildedrose.item.Item;
import com.gildedrose.item.CustomisedItemFactory;
import com.gildedrose.item.QualityValues;

class GildedRose {

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            customisedItem(item).updateState();
            if (hasReachedLowestQualityValue(item)) {
                item.quality = QualityValues.lowestValuePossible();
            } else if (hasReachedHighestQualityValue(item)) {
                item.quality = QualityValues.highestValuePossible(item);
            }
        }
    }

    private CustomisedItem customisedItem(Item item) {
        return new CustomisedItemFactory(item).customiseItem(item);
    }

    private boolean hasReachedLowestQualityValue(Item item) {
        return item.quality < QualityValues.lowestValuePossible();
    }

    private boolean hasReachedHighestQualityValue(Item item) {
        return item.quality > QualityValues.highestValuePossible(item);
    }
}