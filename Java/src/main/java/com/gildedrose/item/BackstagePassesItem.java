package com.gildedrose.item;

public class BackstagePassesItem extends CustomisedItem {

    private final Item item;

    public BackstagePassesItem(Item item) {
        this.item = item;
    }

    @Override
    int updatedItemSellIn() {
        return item.sellIn -= 1;
    }

    @Override
    int updatedItemQuality() {
        if (sellByDayValueIsOver(10)) {
            return qualityIncreasedBy(1);
        } else if (sellByDayValueIsOver(5)) {
            return qualityIncreasedBy(2);
        } else if (sellByDayValueIsOver(0)) {
            return qualityIncreasedBy(3);
        } else {
            return qualityDroppedToZero();
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

    private boolean sellByDayValueIsOver(int dayNumber) {
        return item.sellIn > dayNumber;
    }

    private int qualityIncreasedBy(int qualityValue) {
        return item.quality += qualityValue;
    }

    private int qualityDroppedToZero() {
        return 0;
    }
}
