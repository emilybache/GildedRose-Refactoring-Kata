/* (C)2022 */
package com.gildedrose;

import com.gildedrose.item.Item;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            items[i].updateQuality();
            items[i].updateSellIn();
        }
    }
}
