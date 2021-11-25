package com.gildedrose.item_helpers;

import com.gildedrose.main.Item;
import com.gildedrose.items.AgedBrieItem;
import com.gildedrose.items.BackstagePassItem;
import com.gildedrose.items.ConjuredItem;
import com.gildedrose.items.LegendaryItem;
import com.gildedrose.items.NormalItem;

import static com.gildedrose.item_helpers.ItemHandler.validate;
import static com.gildedrose.item_helpers.ItemName.getItemName;

public class ItemFactory {

    private ItemFactory() {
    }

    public static ItemType getItem(Item item) {
        validate(item);
        ItemName itemName = getItemName(item.name);
        switch (itemName) {
            case AGED_BRIE:
                return new AgedBrieItem(item);
            case LEGENDARY:
                return new LegendaryItem(item);
            case BACKSTAGE_PASS:
                return new BackstagePassItem(item);
            case CONJURED:
                return new ConjuredItem(item);
            default:
                return new NormalItem(item);
        }
    }

}
