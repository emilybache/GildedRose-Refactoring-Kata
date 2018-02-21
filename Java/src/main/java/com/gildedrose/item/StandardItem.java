package com.gildedrose.item;

class StandardItem extends CustomisedItem {

    StandardItem(Item item) {
        super(item);
    }

    @Override
    int updatedItemQuality() {
        if (sellByDayValueIsOverZero()) {
            return item.quality -= 1;
        } else {
            return item.quality -= 2;
        }
    }

    private boolean sellByDayValueIsOverZero() {
        return item.sellIn > 0;
    }
}
