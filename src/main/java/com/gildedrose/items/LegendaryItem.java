package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

import static com.gildedrose.item_helpers.ItemName.LEGENDARY;

public class LegendaryItem implements ItemType {

    public static final int LEGENDARY_ITEM_QUALITY = 80;
    public static final String NOT_LEGENDARY_ITEM_ERROR_MESSAGE = "Item is legendary, quality must be always 80! Current value: ";
    private final ItemHandler item;

    public LegendaryItem(Item item) {
        validate(item);
        this.item = new ItemHandler(item);
    }

    @Override
    public void updateQuality() {
        item.decrementSellInDate();
    }

    private void validate(Item item) {
        if (notLegendary(item)) {
            throw new IllegalArgumentException(NOT_LEGENDARY_ITEM_ERROR_MESSAGE + item.quality);
        }
    }

    public boolean notLegendary(Item item) {
        return item.name.equals(LEGENDARY.toString()) && item.quality != LEGENDARY_ITEM_QUALITY;
    }

}
