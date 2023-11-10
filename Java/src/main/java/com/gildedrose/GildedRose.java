package com.gildedrose;

class GildedRose {
    Item[] items;

    public static String backStagePasses = "Backstage passes to a TAFKAL80ETC concert";
    public static String sulfuras = "Sulfuras, Hand of Ragnaros";
    public static String agedBrie = "Aged Brie";

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            if (!item.name.equals(agedBrie) && !item.name.equals(backStagePasses)) {
                if (item.quality > 0) {
                    if (!item.name.equals(sulfuras)) {
                        item.quality = item.quality - 1;
                    }
                }
            } else {
                if (item.quality < 50) {
                    item.quality = item.quality + 1;

                    if (item.name.equals(backStagePasses)) {
                        if (item.sellIn < 11) {
                            if (item.quality < 50) {
                                item.quality = item.quality + 1;
                            }
                        }

                        if (item.sellIn < 6) {
                            if (item.quality < 50) {
                                item.quality = item.quality + 1;
                            }
                        }
                    }
                }
            }

            if (!item.name.equals(sulfuras)) {
                item.sellIn = item.sellIn - 1;
            }

            if (item.sellIn < 0) {
                if (!item.name.equals(agedBrie)) {
                    if (!item.name.equals(backStagePasses)) {
                        if (item.quality > 0) {
                            if (!item.name.equals(sulfuras)) {
                                item.quality = item.quality - 1;
                            }
                        }
                    } else {
                        item.quality = 0;
                    }
                } else {
                    if (item.quality < 50) {
                        item.quality = item.quality + 1;
                    }
                }
            }
        }
    }
}
