package com.gildedrose.item;

class AgedBrie extends CustomisedItem {

    AgedBrie(Item item) {
        super(item);
    }

    @Override
    int updatedItemSellIn() {
        return item.sellIn -= 1;
    }

    @Override
    int updatedItemQuality() {
        return item.quality += 1;
    }
}
