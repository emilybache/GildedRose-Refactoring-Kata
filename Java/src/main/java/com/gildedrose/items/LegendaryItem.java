package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;

public class LegendaryItem implements ItemType {

    private final ItemHandler item;

    public LegendaryItem(Item item) {
        this.item = new ItemHandler(item);
    }

    @Override
    public void updateQuality() {
        item.decrementSellInDate();
        item.setLegendaryQuality();
    }

}
