package com.gildedrose;

class GildedRose {
    public Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
        	System.out.println(items[i].getClass());
        	items[i] = items[i].updateQuality(items[i]);
        }
    }
}