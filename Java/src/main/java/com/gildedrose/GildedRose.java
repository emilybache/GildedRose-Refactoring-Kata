package com.gildedrose;

import com.gildedrose.item.Item;

class GildedRose {

    public static final String AGED_BRIE = "Aged Brie";
    public static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    public static final String SULFURAS = "Sulfuras, Hand of Ragnaros";

    public static final int MINIMUM_QUALITY = 0;
    public static final int MAXIMUM_QUALITY = 50;

    public static final int SELL_IN_DAY11 = 11;
    public static final int SELL_IN_DAY6 = 6;
    public static final int SELL_IN_EXPIRED = 0;

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            doUpdateQuality(item);
        }
    }

    private void doUpdateQuality(Item item) {
        boolean isAgedBrie = item.name.equals(AGED_BRIE);
        boolean isBackstagePasses = item.name.equals(BACKSTAGE_PASSES);
        boolean isSulfuras = item.name.equals(SULFURAS);

        if (isAgedBrie) {
            // processing quality
            if (item.quality < MAXIMUM_QUALITY) {
                increaseQuality(item);
            }

            // processing sell date
            decreaseDay(item);

            // processing sell date
            if (isExpired(item)) {
                if (item.quality < MAXIMUM_QUALITY) {
                    increaseQuality(item);
                }
            }
        } else if (isBackstagePasses) {
            // processing quality
            if (item.quality < MAXIMUM_QUALITY) {
                increaseQuality(item);

                // processing sell date
                if (item.sellIn < SELL_IN_DAY11) {
                    if (item.quality < MAXIMUM_QUALITY) {
                        increaseQuality(item);
                    }
                }

                if (item.sellIn < SELL_IN_DAY6) {
                    if (item.quality < MAXIMUM_QUALITY) {
                        increaseQuality(item);
                    }
                }
            }

            // processing sell date
            decreaseDay(item);

            // processing sell date
            if (isExpired(item)) {
                item.quality = MINIMUM_QUALITY;
            }
        } else if (isSulfuras) {
            //sulfuras is doing nothing

        } else {
            //standard item
            if (item.quality > MINIMUM_QUALITY) {
                decreaseQuality(item);
            }

            // processing sell date
            decreaseDay(item);

            // processing sell date
            if (isExpired(item)) {
                // processing quality
                if (item.quality > MINIMUM_QUALITY) {
                    decreaseQuality(item);
                }
            }
        }
    }

    private void decreaseDay(Item item) {
        item.sellIn = item.sellIn - 1;
    }

    private boolean isExpired(Item item) {
        return item.sellIn < SELL_IN_EXPIRED;
    }

    private void decreaseQuality(Item item) {
        item.quality = item.quality - 1;
    }

    private void increaseQuality(Item item) {
        item.quality = item.quality + 1;
    }

}
