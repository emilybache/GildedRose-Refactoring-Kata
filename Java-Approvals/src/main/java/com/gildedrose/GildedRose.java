package com.gildedrose;

class GildedRose {

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            if (isAgedBride(item)) {
                item.increaseQualityByOne();
            } else if (isBackstagePasses(item)) {
                item.increaseQualityBackstage();
            } else {
                item.decreaseQualityByOne();
            }

            item.decreaseSellInEachDay();

            if (item.sellIn < 0) {
                if (isAgedBride(item)) {
                    item.increaseQualityByOne();
                } else if (isBackstagePasses(item)) {
                    item.quality = 0;
                } else {
                    item.decreaseQualityByOne();
                }
            }
        }
    }

    private static boolean isBackstagePasses(Item item) {
        return item.name.equals(Item.BACKSTAGE_PASSES);
    }

    private static boolean isAgedBride(Item item) {
        return item.name.equals(Item.AGED_BRIE);
    }

}
