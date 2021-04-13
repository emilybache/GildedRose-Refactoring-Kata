package com.gildedrose;

import com.gildedrose.behavior.ItemBehaviorPicker;

import java.util.Arrays;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        Arrays.stream(items).forEach(item -> ItemBehaviorPicker.forName(item.name).processUpdate(item));
    }
}