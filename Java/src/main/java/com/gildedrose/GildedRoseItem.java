package com.gildedrose;

import com.sun.xml.internal.rngom.parse.host.Base;

/**
 * Class which update quality for all items
 */
class GildedRoseItem  {
    Item[] items;
    public static final String SULFURA = "Sulfuras, Hand of Ragnaros";
    private ItemFactory itemFactory;

    public GildedRoseItem(Item[] items) {
        this.items = items;
        itemFactory = new ItemFactory();
    }

    private void updateNumberOfdayToSellRemaining(Item item) {
        item.sellIn -= 1;
    }


    public void updateQuality() {
        for (Item item : items) {
            if (item.name.equals(SULFURA)) {
                continue;
            }
            ItemInterface typeItem = itemFactory.createItemType(item);
            updateNumberOfdayToSellRemaining(item);
            typeItem.updateQuality();
        }
    }
}
