package com.gildedrose;

class GildedRose {
    private static final String AGED_BRIE = "Aged Brie";
    private static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    private static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    private static final int MIN_QUALITY = 0;
    private static final int MAX_QUALITY = 50;
    private static final int BACKSTAGE_DOUBLE_INCREASE_DAYS = 10;
    private static final int BACKSTAGE_TRIPLE_INCREASE_DAYS = 5;

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            updateItem(item);
        }
    }

    private void updateItem(Item item) {
        if (item.name.equals(SULFURAS)) {
            return;
        }

        if (item.name.equals(AGED_BRIE)) {
            increaseQuality(item);
        } else if (item.name.equals(BACKSTAGE_PASSES)) {
            increaseQuality(item);
            if (item.sellIn <= BACKSTAGE_DOUBLE_INCREASE_DAYS) {
                increaseQuality(item);
            }
            if (item.sellIn <= BACKSTAGE_TRIPLE_INCREASE_DAYS) {
                increaseQuality(item);
            }
        } else {
            decreaseQuality(item);
        }

        item.sellIn = item.sellIn - 1;

        if (item.sellIn < 0) {
            if (item.name.equals(AGED_BRIE)) {
                increaseQuality(item);
            } else if (item.name.equals(BACKSTAGE_PASSES)) {
                item.quality = MIN_QUALITY;
            } else {
                decreaseQuality(item);
            }
        }
    }

    private void increaseQuality(Item item) {
        if (item.quality < MAX_QUALITY) {
            item.quality = item.quality + 1;
        }
    }

    private void decreaseQuality(Item item) {
        if (item.quality > MIN_QUALITY) {
            item.quality = item.quality - 1;
        }
    }
}
