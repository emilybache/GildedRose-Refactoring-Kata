package com.gildedrose.item;

class Sulfuras extends CustomisedItem {

    private final Item item;

    Sulfuras(Item item) {
        super(item);
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
}
