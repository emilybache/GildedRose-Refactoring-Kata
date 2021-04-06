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
            final Item currentItem = items[i];
            if (!currentItem.name.equals(ITEM_AGED_BRIE)
                    && !currentItem.name.equals(ITEM_BACKSTAGE_PASSES)) {
                if (currentItem.quality > MIN_QUALITY_LEVEL) {
                    if (!currentItem.name.equals(ITEM_SULFURAS_HAND_OF_RAGNAROS)) {
                        currentItem.quality = currentItem.quality - 1;
                    }
                }
            } else {
                if (currentItem.quality < MAX_QUALITY_LEVEL) {
                    currentItem.quality = currentItem.quality + 1;

                    if (currentItem.name.equals(ITEM_BACKSTAGE_PASSES)) {
                        if (currentItem.sellIn < 11) {
                            if (currentItem.quality < MAX_QUALITY_LEVEL) {
                                currentItem.quality = currentItem.quality + 1;
                            }
                        }

                        if (currentItem.sellIn < 6) {
                            if (currentItem.quality < MAX_QUALITY_LEVEL) {
                                currentItem.quality = currentItem.quality + 1;
                            }
                        }
                    }
                }
            }

            if (!currentItem.name.equals(ITEM_SULFURAS_HAND_OF_RAGNAROS)) {
                currentItem.sellIn = currentItem.sellIn - 1;
            }

            if (currentItem.sellIn < 0) {
                if (!currentItem.name.equals(ITEM_AGED_BRIE)) {
                    if (!currentItem.name.equals(ITEM_BACKSTAGE_PASSES)) {
                        if (currentItem.quality > MIN_QUALITY_LEVEL) {
                            if (!currentItem.name.equals(ITEM_SULFURAS_HAND_OF_RAGNAROS)) {
                                currentItem.quality = currentItem.quality - 1;
                            }
                        }
                    } else {
                        currentItem.quality = currentItem.quality - currentItem.quality;
                    }
                } else {
                    if (currentItem.quality < MAX_QUALITY_LEVEL) {
                        currentItem.quality = currentItem.quality + 1;
                    }
                }
            }
        }
    }
}