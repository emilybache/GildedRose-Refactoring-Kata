package com.gildedrose;

class GildedRose {
    private static String AGED_BRIE = "Aged Brie";
    private static String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    private static String SULFURAS = "Sulfuras, Hand of Ragnaros";
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            if (items[i].name.equals(AGED_BRIE)
                || items[i].name.equals(BACKSTAGE_PASSES)) {
                if (items[i].quality < 50) {
                    items[i].quality = items[i].quality + 1;

                    if (items[i].name.equals(BACKSTAGE_PASSES)) {
                        if (items[i].sellIn < 11) {
                            incrementQualityByOne(i);
                        }

                        if (items[i].sellIn < 6) {
                            incrementQualityByOne(i);
                        }
                    }
                }
            } else {
                if (items[i].quality > 0) {
                    if (!items[i].name.equals(SULFURAS)) {
                        decreaseQualityByOne(i);
                    }
                }
            }

            if (!items[i].name.equals(SULFURAS)) {
                items[i].sellIn = items[i].sellIn - 1;
            }

            if (items[i].sellIn < 0) {
                if (!items[i].name.equals(AGED_BRIE)) {
                    if (!items[i].name.equals(BACKSTAGE_PASSES)) {
                        if (items[i].quality > 0) {
                            if (!items[i].name.equals(SULFURAS)) {
                                decreaseQualityByOne(i);
                            }
                        }
                    } else {
                        items[i].quality = 0;
                    }
                } else {
                    incrementQualityByOne(i);
                }
            }
        }
    }

    private void incrementQualityByOne(int i) {
        if (items[i].quality < 50) {
            items[i].quality = items[i].quality + 1;
        }
    }

    private void decreaseQualityByOne(int i) {
        items[i].quality = items[i].quality - 1;
    }

}
