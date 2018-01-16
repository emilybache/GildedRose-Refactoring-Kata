package com.gildedrose.item;

public abstract class CustomisedItem {

    private final Item item;

    CustomisedItem(Item item) {
        this.item = item;
    }

    public final void updateState() {
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

    private boolean hasReachedHighestQualityValue() {
        return item.quality > QualityValues.highestValuePossible(item);
    }

    private boolean hasReachedLowestQualityValue() {
        return item.quality < QualityValues.lowestValuePossible();
    }
}
