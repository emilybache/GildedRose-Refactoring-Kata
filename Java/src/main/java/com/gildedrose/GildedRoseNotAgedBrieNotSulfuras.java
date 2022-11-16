package com.gildedrose;

class GildedRoseNotAgedBrieNotSulfuras extends GildedRoseAbstract {

    public GildedRoseNotAgedBrieNotSulfuras(Item[] itemsList) {
        this.super(itemsList);
    }

    public void updateQuality() {
        for (Item itemValue : itemsList) {
            if (itemValue.sellIn < 0) {
                if (!itemValue.name.equals(AGED_BRIE)) {
                    if (!itemValue.name.equals(BACKSTAGE_PASSES)
                        && (itemValue.quality > 0
                        && !itemValue.name.equals(SULFURAS))) {
                        itemValue.quality --;
                    } else {
                        itemValue.quality -= itemValue.quality;
                    }
                } else if (itemValue.quality < 50) {
                        itemValue.quality ++;
                }
            }
        }
    }
}
