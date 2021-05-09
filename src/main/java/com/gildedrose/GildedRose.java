package com.gildedrose;


import com.gildedrose.updater.UpdaterFactory;
import com.gildedrose.utils.EncapsulatedItem;

import java.util.Objects;


class GildedRose {
    final EncapsulatedItem[] items;

    public GildedRose(final EncapsulatedItem[] items) {
        Objects.requireNonNull(items, "Null EncapsulateItem[] items");
        this.items = items.clone();
    }
    public void updateQuality() {
        for(final EncapsulatedItem item: items){
            UpdaterFactory.getUpdater(item).update();
        }
    }
}