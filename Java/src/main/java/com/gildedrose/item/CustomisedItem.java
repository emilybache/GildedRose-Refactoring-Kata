package com.gildedrose.item;

public abstract class CustomisedItem {

    public final Item item;

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

    int updatedItemSellIn() {
        return item.sellIn -= 1;
    }

    abstract int updatedItemQuality();

    private boolean hasReachedHighestQualityValue() {
        return item.quality > QualityValues.highestValuePossible(item);
    }

    private boolean hasReachedLowestQualityValue() {
        return item.quality < QualityValues.lowestValuePossible();
    }
}
