package com.gildedrose.item;

class BackstagePassesItem extends CustomisedItem {

    BackstagePassesItem(Item item) {
        super(item);
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
