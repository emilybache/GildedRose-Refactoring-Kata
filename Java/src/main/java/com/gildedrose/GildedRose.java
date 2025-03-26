package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    /**
     * Update the quality of the items
     */
    public void updateQuality() {
        for (Item item : items) {
            updateItemQuality(item);
            updateItemSellIn(item);
            handleExpiredItem(item);
        }
    }

    /**
     * Update the quality of the item
     * @param item the item to update
     */
    private void updateItemQuality(Item item) {
        if (isSpecialItem(item)) {
            increaseQuality(item);
            if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                handleBackstagePasses(item);
            }
        } else {
            decreaseQuality(item);
        }
    }

    /**
     * Update the sellIn of the item
     * @param item the item to update
     */
    private void updateItemSellIn(Item item) {
        if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
            item.sellIn--;
        }
    }

    /**
     * Handle the expired item
     * @param item the item to handle
     */
    private void handleExpiredItem(Item item) {
        if (item.sellIn < 0) {
            if (item.name.equals("Aged Brie")) {
                increaseQuality(item);
            } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                item.quality = 0;
            } else {
                decreaseQuality(item);
            }
        }
    }

    /**
     * Check if the item is a special item
     * @param item the item to check
     * @return true if the item is a special item, false otherwise
     */
    private boolean isSpecialItem(Item item) {
        return item.name.equals("Aged Brie") || item.name.equals("Backstage passes to a TAFKAL80ETC concert");
    }

    /**
     * Increase the quality of the item
     * @param item the item to increase the quality
     */
    private void increaseQuality(Item item) {
        if (item.quality < 50) {
            item.quality++;
        }
    }

    /**
     * Decrease the quality of the item
     * @param item the item to decrease the quality
     */
    private void decreaseQuality(Item item) {
        if (item.quality > 0 && !item.name.equals("Sulfuras, Hand of Ragnaros")) {
            item.quality--;
        }
    }

    /**
     * Handle the backstage passes
     * @param item the item to handle
     */
    private void handleBackstagePasses(Item item) {
        if (item.sellIn < 11) {
            increaseQuality(item);
        }
        if (item.sellIn < 6) {
            increaseQuality(item);
        }
    }
}
