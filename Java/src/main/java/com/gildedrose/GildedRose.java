package com.gildedrose;

class GildedRose {
    public static final String ITEM_AGED_BRIE = "Aged Brie";
    public static final String ITEM_SULFURAS_HAND_OF_RAGNAROS = "Sulfuras, Hand of Ragnaros";
    public static final String ITEM_BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    public static final int MAX_QUALITY_LEVEL = 50;
    public static final int MIN_QUALITY_LEVEL = 0;

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            if (!items[i].name.equals(ITEM_AGED_BRIE)
                    && !items[i].name.equals(ITEM_BACKSTAGE_PASSES)) {
                if (items[i].quality > MIN_QUALITY_LEVEL) {
                    if (!items[i].name.equals(ITEM_SULFURAS_HAND_OF_RAGNAROS)) {
                        items[i].quality = items[i].quality - 1;
                    }
                }
            } else {
                if (items[i].quality < MAX_QUALITY_LEVEL) {
                    items[i].quality = items[i].quality + 1;

                    if (items[i].name.equals(ITEM_BACKSTAGE_PASSES)) {
                        if (items[i].sellIn < 11) {
                            if (items[i].quality < MAX_QUALITY_LEVEL) {
                                items[i].quality = items[i].quality + 1;
                            }
                        }

                        if (items[i].sellIn < 6) {
                            if (items[i].quality < MAX_QUALITY_LEVEL) {
                                items[i].quality = items[i].quality + 1;
                            }
                        }
                    }
                }
            }

            if (!items[i].name.equals(ITEM_SULFURAS_HAND_OF_RAGNAROS)) {
                items[i].sellIn = items[i].sellIn - 1;
            }

            if (items[i].sellIn < 0) {
                if (!items[i].name.equals(ITEM_AGED_BRIE)) {
                    if (!items[i].name.equals(ITEM_BACKSTAGE_PASSES)) {
                        if (items[i].quality > MIN_QUALITY_LEVEL) {
                            if (!items[i].name.equals(ITEM_SULFURAS_HAND_OF_RAGNAROS)) {
                                items[i].quality = items[i].quality - 1;
                            }
                        }
                    } else {
                        items[i].quality = items[i].quality - items[i].quality;
                    }
                } else {
                    if (items[i].quality < MAX_QUALITY_LEVEL) {
                        items[i].quality = items[i].quality + 1;
                    }
                }
            }
        }
    }
}