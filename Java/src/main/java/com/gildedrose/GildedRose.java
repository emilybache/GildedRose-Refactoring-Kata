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
            if (item.quality < 50) {
                item.quality++;
            }
        } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            if (item.sellIn < 11) {
                if (item.quality < 50) {
                    item.quality++;
                }
            }

            if (item.sellIn < 6) {
                if (item.quality < 50) {
                    item.quality++;
                }
            }
        } else if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
            return;
        } else if (item.quality > 0) {
            item.quality--;
        }
    }

    private static void updateSelling(Item item) {
        if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
            item.sellIn--;
        }
    }

    private static boolean isExpired(Item item) {
        return item.sellIn < 0;
    }

    private static void updateExpired(Item item) {
        if (!item.name.equals("Aged Brie")) {
            if (!item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                if (item.quality > 0) {
                    if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                        item.quality--;
                    }
                }
            } else {
                item.quality = 0;
            }
        } else {
            if (item.quality < 50) {
                item.quality++;
            }
        }
    }
}
