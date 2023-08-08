package com.gildedrose;

public class GildedRose {
    private static final int MAX_QUALITY = 50;
    private static final int MIN_QUALITY = 0;

    public static void updateQuality(Item[] items) {
        for (Item item : items) {
            if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                item.sellIn--; // Decrease sellIn for all items except Sulfuras

                if (item.name.equals("Aged Brie")) {
                    increaseQuality(item);
                    if (item.sellIn < 0) {
                        increaseQuality(item); // Quality increases twice after sellIn passes
                    }
                } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                    increaseQuality(item);
                    if (item.sellIn < 11) {
                        increaseQuality(item); // Quality increases by 2 when sellIn is 10 or less
                    }
                    if (item.sellIn < 6) {
                        increaseQuality(item); // Quality increases by 3 when sellIn is 5 or less
                    }
                    if (item.sellIn < 0) {
                        item.quality = MIN_QUALITY; // Quality drops to 0 after the concert
                    }
                } else if (item.name.equals("Conjured Mana Cake")) {
                    decreaseQuality(item, 2); // Quality decreases twice as fast for conjured items
                } else {
                    decreaseQuality(item); // Normal items decrease in quality
                    if (item.sellIn < 0) {
                        decreaseQuality(item); // Quality decreases twice after sellIn passes
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
