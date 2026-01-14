package com.gildedrose;

class GildedRose {

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {

            if (item.isAgedBride()) {
                item.increaseQualityByOne();

                item.decreaseSellInEachDay();

                if (item.sellIn < 0) {
                    item.increaseQualityByOne();
                }
            } else {
                if (item.isBackstagePasses()) {
                    item.increaseQualityBackstage();

                    item.decreaseSellInEachDay();

                    if (item.sellIn < 0) {
                        item.quality = 0;
                    }
                } else {
                    item.decreaseQualityByOne();

                    item.decreaseSellInEachDay();

                    if (item.sellIn < 0) {
                        item.decreaseQualityByOne();
                    }
                }
            }
        }
    }

}
