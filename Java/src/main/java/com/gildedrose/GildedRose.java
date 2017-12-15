package com.gildedrose;

import com.gildedrose.item.Item;
import com.gildedrose.item.ItemFactory;

class GildedRose {

    private static final int LOWEST_QUALITY_LEVEL_POSSIBLE = 0;
    private static final int HIGHEST_QUALITY_LEVEL_POSSIBLE = 50;
    private final ItemFactory itemFactory;
    Item[] items;

    public GildedRose(Item[] items) {
        this.itemFactory = new ItemFactory();
        this.items = items;
    }

    public void updateQuality() {
        customizeItems();
        for (Item item : items) {
            item.updateYourState();
            if (hasReachedLowestQualityLimit(item.quality)) {
                item.quality = LOWEST_QUALITY_LEVEL_POSSIBLE;
            } else if (hasReachedHighestQualityLimit(item.quality)) {
                item.quality = HIGHEST_QUALITY_LEVEL_POSSIBLE;
            }
        }
    }

    private void customizeItems() {
        for (Item item : items) {
            items = new Item[]{itemFactory.createItem(item.name, item.sellIn, item.quality)};
        }
    }

    private boolean hasReachedLowestQualityLimit(int itemQuality) {
        return itemQuality < LOWEST_QUALITY_LEVEL_POSSIBLE;
    }

    private boolean hasReachedHighestQualityLimit(int itemQuality) {
        return itemQuality > HIGHEST_QUALITY_LEVEL_POSSIBLE;
    }
}