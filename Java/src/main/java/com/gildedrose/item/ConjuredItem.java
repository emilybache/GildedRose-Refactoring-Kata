package com.gildedrose.item;

class ConjuredItem extends CustomisedItem {

    ConjuredItem(Item item) {
        super(item);
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
