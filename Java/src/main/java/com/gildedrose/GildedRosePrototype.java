package com.gildedrose;

class GildedRosePrototype {
    Item[] items;

    public GildedRosePrototype(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            ItemUpdaterFactory.getItemUpdater(item)
                    .updateStateFor(item);
        }
    }
}