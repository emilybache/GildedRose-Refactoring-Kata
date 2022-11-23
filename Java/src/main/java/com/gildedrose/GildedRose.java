package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateStorage() {
        for (Item item : items) {
            new StrorageItem(item).dailyUpdateItem(item);
        }
    }
}
