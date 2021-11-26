package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

import static com.gildedrose.item_helpers.ItemName.LEGENDARY;

public class LegendaryItem implements ItemType {

    public static final int LEGENDARY_ITEM_QUALITY = 80;

    private final ItemHandler item;

    public LegendaryItem(Item item) {
        this.item = new ItemHandler(item);
    }

    @Override
    public void updateQuality() {
        item.decrementSellInDate();
    }

    public static boolean isLegendary(Item item) {
        return item.name.equals(LEGENDARY.toString());
    }

    public static boolean isNotLegendary(Item item) {
        return !item.name.equals(LEGENDARY.toString());
    }

}
