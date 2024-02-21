package com.gildedrose;

import java.util.Arrays;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        Arrays.stream(items).forEach(this::updateItemQuality);
    }

    private void updateItemQuality(Item item) {
        switch (item.name) {
            case "Aged Brie":
                increaseQuality(item);
                break;
            case "Backstage passes to a TAFKAL80ETC concert":
                handleBackstagePass(item);
                break;
            case "Sulfuras, Hand of Ragnaros":
                // Legendary item, no changes needed
                break;
            case "Conjured":
                decreaseQuality(item, 2);
                break;
            default:
                decreaseQuality(item);
                break;
        }

        if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
            item.sellIn -= 1;
        }

        if (item.sellIn < 0) {
            handleExpiredItem(item);
        }
    }

    private void increaseQuality(Item item) {
        if (item.quality < 50) {
            item.quality += 1;
        }
    }

    private void decreaseQuality(Item item) {
        if (item.quality > 0) {
            item.quality -= 1;
        }
    }

    private void decreaseQuality(Item item, int factor) {
        item.quality = Math.max(0, item.quality - factor);
    }

    private void handleBackstagePass(Item item) {
        if (item.quality < 50) {
            item.quality += 1;
            if (item.sellIn < 11) {
                increaseQuality(item);
            }
            if (item.sellIn < 6) {
                increaseQuality(item);
            }
        }
    }

    private void handleExpiredItem(Item item) {
        if (!item.name.equals("Aged Brie") && !item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                decreaseQuality(item);
                if (item.name.equals("Conjured")) {
                    decreaseQuality(item, 1); // Additional decrease for Conjured items
                }
            }
        } else {
            item.quality = 0;
        }
    }
}	
