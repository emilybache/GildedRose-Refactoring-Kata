package com.gildedrose;

class GildedRose {
    private final static String AGED_BRIE = "Aged Brie";
    private final static String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    private final static String SULFURAS = "Sulfuras, Hand of Ragnaros";

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            if (items[i].name.equals(AGED_BRIE)) {
                increaseQualityByOne(i);
            } else if (items[i].name.equals(BACKSTAGE_PASSES)) {
                if (items[i].sellIn < 6) {
                    increaseQualityByThree(i);
                } else if (items[i].sellIn < 11) {
                    increaseQualityByTwo(i);
                } else {
                    increaseQualityByOne(i);
                }
            } else {
                decreaseQualityByOne(i);
            }

            if (!items[i].name.equals(SULFURAS)) {
                items[i].sellIn = items[i].sellIn - 1;
            }

            if (items[i].sellIn < 0) {
                if (items[i].name.equals(AGED_BRIE)) {
                    increaseQualityByOne(i);
                } else {
                    if (items[i].name.equals(BACKSTAGE_PASSES)) {
                        items[i].quality = 0;
                    } else {
                        decreaseQualityByOne(i);
                    }
                }
            }
        }
    }

    private void decreaseQualityByOne(int i) {
        if (items[i].quality > 0) {
            if (!items[i].name.equals(SULFURAS)) {
                items[i].quality = items[i].quality - 1;
            }
        }
    }

    private void increaseQualityByOne(int i) {
        if (items[i].quality < 50) {
            items[i].quality = items[i].quality + 1;
        }
    }

    private void increaseQualityByTwo(int i) {
        increaseQualityByOne(i);
        increaseQualityByOne(i);
    }

    private void increaseQualityByThree(int i) {
        increaseQualityByTwo(i);
        increaseQualityByOne(i);
    }

}
