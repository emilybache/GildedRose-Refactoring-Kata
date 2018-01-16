package com.gildedrose.item;

class StandardItem extends CustomisedItem {

    private final Item item;

    StandardItem(Item item) {
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
            return item.quality -= 1;
        } else {
            return item.quality -= 2;
        }
    }

    private boolean sellByDayValueIsOverZero() {
        return item.sellIn > 0;
    }
}
