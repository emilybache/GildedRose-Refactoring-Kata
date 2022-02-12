package com.gildedrose;

class GildedRose {
    Item[] items;
    ItemHolder[] itemHolders;

    public GildedRose(Item[] items) {
        this.items = items;
        this.itemHolders = new ItemHolder[items.length];
        for (int i=0; i<items.length; i++) {
            itemHolders[i] = ItemHolderFactory.createItemHolder(this.items[i]);
        }
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            itemHolders[i].update();
        }
    }
}
