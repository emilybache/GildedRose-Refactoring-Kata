package com.gildedrose.item_helpers;

import com.gildedrose.Item;
import com.gildedrose.items.AgedBrie;
import com.gildedrose.items.BackstagePass;
import com.gildedrose.items.Conjured;
import com.gildedrose.items.Legendary;
import com.gildedrose.items.Normal;

import static com.gildedrose.item_helpers.ItemName.getItemName;

public class ItemFactory {

    private ItemFactory() {
    }

    public static ItemType getItem(Item item) {
        ItemName itemName = getItemName(item.name);
        switch (itemName) {
            case AGED_BRIE:
                return new AgedBrie(item);
            case LEGENDARY:
                return new Legendary(item);
            case BACKSTAGE_PASS:
                return new BackstagePass(item);
            case CONJURED:
                return new Conjured(item);
            default:
                return new Normal(item);
        }
    }
}
