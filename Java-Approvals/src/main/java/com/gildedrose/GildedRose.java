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
                boolean isBackstagePasses = item.isBackstagePasses();
                if (isBackstagePasses) {
                    item.increaseQualityBackstage();
                } else {
                    item.decreaseQualityByOne();
                }

                item.decreaseSellInEachDay();

                if (item.sellIn < 0) {
                    if (isBackstagePasses) {
                        item.quality = 0;
                    } else {
                        item.decreaseQualityByOne();
                    }
                }
            }
        }
    }

}
