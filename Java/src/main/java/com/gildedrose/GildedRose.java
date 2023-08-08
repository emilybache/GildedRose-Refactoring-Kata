package com.gildedrose;

public class GildedRose {
    private static final int MAX_QUALITY = 50;
    private static final int MIN_QUALITY = 0;

    public static void updateQuality(Item[] items) {
        for (Item item : items) {
            if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                item.sellIn--;

                if (item.name.equals("Aged Brie")) {
                    increaseQuality(item);
                    if (item.sellIn < 0) {
                        increaseQuality(item);
                    }
                } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                    increaseQuality(item);
                    if (item.sellIn < 11) {
                        increaseQuality(item);
                    }
                    if (item.sellIn < 6) {
                        increaseQuality(item);
                    }
                    if (item.sellIn < 0) {
                        item.quality = MIN_QUALITY;
                    }
                } else if (item.name.equals("Conjured Mana Cake")) {
                    decreaseQuality(item, 2);
                } else {
                    decreaseQuality(item);
                    if (item.sellIn < 0) {
                        decreaseQuality(item);
                    }
                }
            }
        }
    }

    private static void increaseQuality(Item item) {
        if (item.quality < MAX_QUALITY) {
            item.quality++;
        }
    }

    private static void decreaseQuality(Item item) {
        if (item.quality > MIN_QUALITY) {
            item.quality--;
        }
    }

    private static void decreaseQuality(Item item, int amount) {
        for (int i = 0; i < amount; i++) {
            decreaseQuality(item);
        }
    }
}
