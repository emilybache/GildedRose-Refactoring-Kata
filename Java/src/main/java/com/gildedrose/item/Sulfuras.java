package com.gildedrose.item;

public class Sulfuras extends CustomisedItem {

    private final Item item;

    public Sulfuras(Item item) {
        this.item = item;
    }

    @Override
    int updatedItemSellIn() {
        return item.sellIn;
    }

    @Override
    int updatedItemQuality() {
        return item.quality;
    }

    @Override
    protected boolean hasReachedHighestQualityValue() {
        return item.quality > QualityValues.highestValuePossible(item) ;
    }

    @Override
    protected boolean hasReachedLowestQualityValue() {
        return item.quality < QualityValues.lowestValuePossible();
    }
}
