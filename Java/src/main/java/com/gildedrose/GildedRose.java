package com.gildedrose;

import com.gildedrose.items.*;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            GildedItem gi = toGildedItem(items[i]);
            gi.updateQuality();
        }
    }

    private GildedItem toGildedItem(Item item) {
        if (ItemUtils.isAgedBrie(item)) return new AgedBrieItem(item);
        if (ItemUtils.isBackstage(item)) return new BackstageItem(item);
        if (ItemUtils.isSulfuras(item)) return new SulfurasItem(item);
        // this conjured item does not work properly yet
        return new NormalItem(item);
    }
}
