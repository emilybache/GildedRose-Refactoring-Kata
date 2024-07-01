package com.gildedrose;

import com.gildedrose.ItemUpdater;

class GildedRose {
    Item[] items; // Inventory*

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        // Instantiate updaters for each item type
        ItemUpdater defaultUpdater = new DefaultItemUpdater();
        ItemUpdater agedBrieUpdater = new AgedBrieUpdater();
        ItemUpdater backstagePassUpdater = new BackStagePassUpdater();
        ItemUpdater conjuredItemUpdater = new ConjuredItemUpdater();

        // Iterate through items and call corresponding updater
        for (Item item : items) {
            String name = item.name;
            switch (name) {
                case "Aged Brie":
                    agedBrieUpdater.update(item);
                    break;
                case "Backstage passes to a TAFKAL80ETC concert":
                    backstagePassUpdater.update(item);
                    break;
                case "Sulfuras, Hand of Ragnaros":
                    break;
                case "Conjured":
                    conjuredItemUpdater.update(item);
                    break;
                default:
                    defaultUpdater.update(item);
            }
        }
    }
}
