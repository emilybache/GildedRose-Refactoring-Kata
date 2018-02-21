package com.gildedrose.item;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

public class CustomisedItemFactory {

    private final static Map<String, Function<Item, CustomisedItem>> ITEM_TYPES_LIST = new HashMap<>();
    public final static String SULFURAS = "Sulfuras, Hand of Ragnaros";
    public final static String BRIE = "Aged Brie";
    public final static String BACKSTAGE_PASSES_ITEM = "Backstage passes to a TAFKAL80ETC concert";
    public final static String CONJURED_ITEM = "Conjured";

    public CustomisedItemFactory() {
        ITEM_TYPES_LIST.put(SULFURAS, Sulfuras::new);
        ITEM_TYPES_LIST.put(BRIE, AgedBrie::new);
        ITEM_TYPES_LIST.put(BACKSTAGE_PASSES_ITEM, BackstagePassesItem::new);
        ITEM_TYPES_LIST.put(CONJURED_ITEM, ConjuredItem::new);
    }

    public CustomisedItem customiseItem(Item item) {
        String name = item.name;
        if (isStandardItem(name)) {
            return new StandardItem(item);
        }
        return ITEM_TYPES_LIST.get(name).apply(item);
    }

    private boolean isStandardItem(String name) {
        return !ITEM_TYPES_LIST.keySet().contains(name);
    }
}
