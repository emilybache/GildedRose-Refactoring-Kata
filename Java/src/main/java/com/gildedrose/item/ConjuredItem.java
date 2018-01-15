package com.gildedrose.item;

public class ConjuredItem extends CustomisedItem {

    private final Item item;

    public ConjuredItem(Item item) {
        this.item = item;
    }

    @Override
    protected boolean hasReachedHighestQualityValue() {
        return item.quality > QualityValues.highestValuePossible(item);
    }

    @Override
    protected boolean hasReachedLowestQualityValue() {
        return item.quality < QualityValues.lowestValuePossible();
    }

    @Override
    int updatedItemSellIn() {
        return item.sellIn -= 1;
    }

    @Override
    int updatedItemQuality() {
        if (sellByDayValueIsOverZero()) {
            return item.quality -= 2;
        } else {
            return item.quality -= 4;
        }
    }

    private boolean sellByDayValueIsOverZero() {
        return item.sellIn > 0;
    }
}
