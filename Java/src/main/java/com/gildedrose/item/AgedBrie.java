package com.gildedrose.item;

public class AgedBrie extends CustomisedItem {

    private final Item item;

    public AgedBrie(Item item) {
        this.item = item;
    }

    @Override
    int updatedItemSellIn() {
        return item.sellIn -= 1;
    }

    @Override
    int updatedItemQuality() {
        return item.quality += 1;
    }

    @Override
    protected boolean hasReachedHighestQualityValue() {
        return item.quality > QualityValues.highestValuePossible(item);
    }

    @Override
    protected boolean hasReachedLowestQualityValue() {
        return item.quality < QualityValues.lowestValuePossible();
    }
}
