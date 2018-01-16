package com.gildedrose.item;

public abstract class CustomisedItem {

    public final void updateState(Item item) {
        item.sellIn = updatedItemSellIn();
        item.quality = updatedItemQuality();

        if (hasReachedLowestQualityValue(item)) {
            item.quality = QualityValues.lowestValuePossible();
        } else if (hasReachedHighestQualityValue(item)) {
            item.quality = QualityValues.highestValuePossible(item);
        }
    }

    abstract int updatedItemSellIn();

    abstract int updatedItemQuality();

    private boolean hasReachedHighestQualityValue(Item item) {
        return item.quality > QualityValues.highestValuePossible(item);
    }

    private boolean hasReachedLowestQualityValue(Item item) {
        return item.quality < QualityValues.lowestValuePossible();
    }
}
