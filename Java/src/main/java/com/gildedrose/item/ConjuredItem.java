package com.gildedrose.item;

class ConjuredItem extends CustomisedItem {

    private final Item item;

    ConjuredItem(Item item) {
        super(item);
        this.item = item;
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
