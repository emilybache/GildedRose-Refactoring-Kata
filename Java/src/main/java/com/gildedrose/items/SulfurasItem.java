package com.gildedrose.items;

import com.gildedrose.Item;

public class SulfurasItem extends GildedItem {
    public SulfurasItem(Item item) { super(item); }

    @Override
    public void updateQuality() {
        // Sulfuras ne change jamais
    }
}

