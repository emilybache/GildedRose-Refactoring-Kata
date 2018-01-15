package com.gildedrose.item;

public abstract class CustomisedItem {

    public final void updateState(Item item) {
        item.sellIn = updatedItemSellIn();
        item.quality = updatedItemQuality();

        if (hasReachedLowestQualityValue()) {
            item.quality = QualityValues.lowestValuePossible();
        } else if (hasReachedHighestQualityValue()) {
            item.quality = QualityValues.highestValuePossible(item);
        }
    }

    abstract int updatedItemSellIn();

    abstract int updatedItemQuality();

    protected abstract boolean hasReachedHighestQualityValue();

    protected abstract boolean hasReachedLowestQualityValue();
}
