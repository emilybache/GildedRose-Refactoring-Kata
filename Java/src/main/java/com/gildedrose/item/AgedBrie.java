package com.gildedrose.item;

class AgedBrie extends CustomisedItem {

    private final Item item;

    AgedBrie(Item item) {
        this.item = item;
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
