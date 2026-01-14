package com.gildedrose;

class GildedRose {

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            if (item.name.equals(Item.AGED_BRIE)) {
                item.increaseQualityByOne();
            } else if (item.name.equals(Item.BACKSTAGE_PASSES)) {
                item.increaseQualityBackstage();
            } else {
                item.decreaseQualityByOne();
            }

            item.decreaseSellInEachDay();

            if (item.sellIn < 0) {
                if (item.name.equals(Item.AGED_BRIE)) {
                    item.increaseQualityByOne();
                } else if (item.name.equals(Item.BACKSTAGE_PASSES)) {
                    item.quality = 0;
                } else {
                    item.decreaseQualityByOne();
                }
            }
        }
    }

}
