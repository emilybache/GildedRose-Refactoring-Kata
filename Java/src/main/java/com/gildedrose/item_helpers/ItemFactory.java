package com.gildedrose.item_helpers;

import com.gildedrose.items.AgedBrieItem;
import com.gildedrose.items.BackstagePassItem;
import com.gildedrose.items.ConjuredItem;
import com.gildedrose.items.LegendaryItem;
import com.gildedrose.items.NormalItem;
import com.gildedrose.main.Item;

import static com.gildedrose.item_helpers.ItemName.getItemName;

public class ItemFactory {

    private ItemFactory() {
    }

    public static ItemType getItemType(Item item) {
        validateQuality(item);
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

    public static final String QUALITY_ERROR_MESSAGE = "Quality cannot be negative! Current value: ";

    private static void validateQuality(Item item) {
        if (qualityIsNegative(item)) {
            throw new IllegalArgumentException(QUALITY_ERROR_MESSAGE + item.quality);
        }
    }

    private static boolean qualityIsNegative(Item item) {
        return item.quality < 0;
    }
}
