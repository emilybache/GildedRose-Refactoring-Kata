package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateInventory() {
        for (Item item : items) {
            updateItem(item);
        }
    }

    private static void updateItem(Item item) {
        updateQuality(item);
        updateSelling(item);
        if (isExpired(item)) {
            updateExpired(item);
        }
    }

    private static void updateQuality(Item item) {
        if (item.name.equals("Aged Brie")) {
            increaseQuality(item);
        } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            increaseQuality(item);

            if (item.sellIn < 11) {
                increaseQuality(item);
            }

            if (item.sellIn < 6) {
                increaseQuality(item);
            }
        } else if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
            return;
        } else decreaseQuality(item);
    }

    private static void updateSelling(Item item) {
        if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
            return;
        }
        item.sellIn--;
    }

    private static boolean isExpired(Item item) {
        return item.sellIn < 0;
    }

    private static void updateExpired(Item item) {
        if (item.name.equals("Aged Brie")) {
            increaseQuality(item);
        } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            item.quality = 0;
        } else if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
            return;
        } else {
            decreaseQuality(item);
        }
    }

    private static void increaseQuality(Item item) {
        if (item.quality < 50) {
            item.quality++;
        }
    }

    private static void decreaseQuality(Item item) {
        if (item.quality > 0) {
            item.quality--;
        }
    }
}
