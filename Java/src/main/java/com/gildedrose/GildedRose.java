package com.gildedrose;

import com.gildedrose.domain.*;
import com.gildedrose.domain.InventoryItem;

import static java.util.stream.Stream.of;

class GildedRose {

    private static final String AGED_BRIE = "Aged Brie";
    private static final String LEGENDARY = "Sulfuras, Hand of Ragnaros";
    private static final String CONJURED_ITEM = "Conjured Mana Cake";
    private static final String BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert";

    Item[] items;

    GildedRose(Item[] items) {
        this.items = items;
    }

    void updateQuality() {
        of(items).forEach(item -> {
            InventoryItem inventoryItem = inventoryFromItem(item);

            // reduce sellIn
            item.sellIn = inventoryItem.reduceSellIn();

            // increase or decrease quality based on the items
            item.quality = inventoryItem.handleQuality();

            if (item.sellIn < 0) {
                // increase or decrease quality more after sell in
                item.quality = inventoryItem.handleQualityAfterSellIn();
            }
        });
    }

    private InventoryItem inventoryFromItem(Item item) {
        switch (item.name) {
            case AGED_BRIE:
                return new AgedBrie(item);
            case LEGENDARY:
                return new Legendary(item);
            case CONJURED_ITEM:
                return new ConjuredItem(item);
            case BACKSTAGE_PASS:
                return new BackstagePass(item);
            default:
                return new DefaultItem(item);
        }
    }
}
