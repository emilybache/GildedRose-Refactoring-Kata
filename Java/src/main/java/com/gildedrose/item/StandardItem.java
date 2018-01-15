package com.gildedrose.item;

public class StandardItem extends CustomisedItem {

    private final Item item;

    public StandardItem(Item item) {
        this.item = item;
    }

    @Override
    int updatedItemSellIn() {
        return item.sellIn -= 1;
    }

    @Override
    int updatedItemQuality() {
        if (sellByDayValueIsOverZero()) {
            return item.quality -= 1;
        } else {
            return item.quality -= 2;
        }
    }

    @Override
    protected boolean hasReachedHighestQualityValue() {
        return item.quality > QualityValues.highestValuePossible(item);
    }

    @Override
    protected boolean hasReachedLowestQualityValue() {
        return item.quality < QualityValues.lowestValuePossible();
    }

    private boolean sellByDayValueIsOverZero() {
        return item.sellIn > 0;
    }
}
