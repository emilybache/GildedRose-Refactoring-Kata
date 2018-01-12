package com.gildedrose;

import com.gildedrose.item.CustomisedItem;
import com.gildedrose.item.Item;
import com.gildedrose.item.CustomisedItemFactory;
import com.gildedrose.item.QualityValues;

class GildedRose {

    private final CustomisedItemFactory itemFactory;
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
        this.itemFactory = new CustomisedItemFactory();
    }

    public void updateQuality() {
        for (Item item : items) {
            itemFactory.customiseItem(item).updateState();
            if (hasReachedLowestQualityValue(item)) {
                item.quality = QualityValues.lowestValuePossible();
            } else if (hasReachedHighestQualityValue(item)) {
                item.quality = QualityValues.highestValuePossible(item);
            }
        }
    }

    private boolean hasReachedLowestQualityValue(Item item) {
        return item.quality < QualityValues.lowestValuePossible();
    }

    private boolean hasReachedHighestQualityValue(Item item) {
        return item.quality > QualityValues.highestValuePossible(item);
    }
}