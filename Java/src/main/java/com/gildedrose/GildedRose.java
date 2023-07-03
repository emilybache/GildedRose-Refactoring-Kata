package com.gildedrose;

import com.gildedrose.business.ItemHandler;
import com.gildedrose.business.ItemHandlerFactory;

class GildedRose {
    Item[] items;
    ItemHandlerFactory itemHandlerFactory;

    public GildedRose(Item[] items) {
        this.items = items;
        this.itemHandlerFactory = new ItemHandlerFactory();
    }

    public void updateQuality() {

        for (Item item: items){
            ItemHandler itemHandler = itemHandlerFactory.createItemHandler(item.name);
            itemHandler.updateItem(item);
        }
    }
}
