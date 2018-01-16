package com.gildedrose.item;

class Sulfuras extends CustomisedItem {

    Sulfuras(Item item) {
        super(item);
    }

    @Override
    int updatedItemSellIn() {
        return item.sellIn;
    }

    @Override
    int updatedItemQuality() {
        return item.quality;
    }
}
