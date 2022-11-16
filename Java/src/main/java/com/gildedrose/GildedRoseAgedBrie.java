package com.gildedrose;

class GildedRoseAgedBrie extends GildedRoseAbstract {

    public GildedRoseAgedBrie(Item[] itemsList) {
        this.super(itemsList);
    }

    public void updateQuality() {
        for (Item itemValue : itemsList) {
            if (!itemValue.name.equals(AGED_BRIE)
                && !itemValue.name.equals(BACKSTAGE_PASSES)
                && itemValue.quality > 0
                && !itemValue.name.equals(SULFURAS)) {
                        itemValue.quality --;
            } else if (itemValue.quality < 50) {
                    itemValue.quality ++;
                    if (itemValue.name.equals(BACKSTAGE_PASSES)
                        && (itemValue.sellIn < 11 || itemValue.sellIn < 6)
                        && itemValue.quality < 50) {
                                itemValue.quality ++;
                    }
                }
        }
    }
}
