package com.gildedrose;

import java.util.Arrays;

class GildedRose {

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public static void updateQuality(GildedRose gildedRose) {
        Arrays.stream(gildedRose.items).forEach(item -> InventoryItem.createInventoryItem(item).age());
    }

}
