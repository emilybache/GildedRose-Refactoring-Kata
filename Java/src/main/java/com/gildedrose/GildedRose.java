package com.gildedrose;

class GildedRose {

    public static final String AGED_BRIE = "Aged Brie";
    public static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    public static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    public static final int QUALITY_LEVEL_0 = 0;
    public static final int QUALITY_LEVEL_50 = 50;
    public static final int SELL_IN_DAY11 = 11;
    public static final int SELL_IN_DAY6 = 6;
    public static final int SELL_IN_DAY0 = 0;

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            doUpdateQuality(item);
        }
    }

    private static void doUpdateQuality(Item item) {
        // processing quality
        boolean isAgedBrie = item.name.equals(AGED_BRIE);
        boolean isBackstagePasses = item.name.equals(BACKSTAGE_PASSES);
        boolean isSulfuras = item.name.equals(SULFURAS);

        if (isAgedBrie) {
            // processing quality
            if (item.quality < QUALITY_LEVEL_50) {
                item.quality = item.quality + 1;
            }

            // processing sell date
            item.sellIn = item.sellIn - 1;

            // processing sell date
            if (item.sellIn < SELL_IN_DAY0) {
                if (item.quality < QUALITY_LEVEL_50) {
                    item.quality = item.quality + 1;
                }
            }
        } else if (isBackstagePasses) {
            // processing quality
            if (item.quality < QUALITY_LEVEL_50) {
                item.quality = item.quality + 1;

                // processing sell date
                if (item.sellIn < SELL_IN_DAY11) {
                    if (item.quality < QUALITY_LEVEL_50) {
                        item.quality = item.quality + 1;
                    }
                }

                if (item.sellIn < SELL_IN_DAY6) {
                    if (item.quality < QUALITY_LEVEL_50) {
                        item.quality = item.quality + 1;
                    }
                }
            }

            // processing sell date
            item.sellIn = item.sellIn - 1;

            // processing sell date
            if (item.sellIn < SELL_IN_DAY0) {
                item.quality = QUALITY_LEVEL_0;
            }
        } else if (isSulfuras) {
            //sulfuras is doing nothing

        } else {
            //standard item
            if (item.quality > QUALITY_LEVEL_0) {
                item.quality = item.quality - 1;
            }

            // processing sell date
            item.sellIn = item.sellIn - 1;

            // processing sell date
            if (item.sellIn < SELL_IN_DAY0) {
                // processing quality
                if (item.quality > QUALITY_LEVEL_0) {
                    item.quality = item.quality - 1;
                }
            }
        }
    }

}
