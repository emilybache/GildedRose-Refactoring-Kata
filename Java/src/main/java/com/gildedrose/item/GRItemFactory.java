package com.gildedrose.item;

import com.gildedrose.Item;

public class GRItemFactory {
    public static final String AGED_BRIE = "Aged Brie";
    public static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    public static final String BACKSTAGE_PASSES = "Backstage passes";
    public static final String CONJURED = "Conjured";

    public static GRItem create(Item item) {
        if (isAgedBrie(item)) {
            return new AgedBrieGRItem(item);
        } else if (isBackstagePasses(item)) {
            return new BackstagePassesGRItem(item);
        } else if (isConjuredItem(item)) {
            return new ConjuredGRItem(item);
        } else if(isSulfuras(item)) {
            return new SulfurasGRItem(item);
        }
        return new RegularGRItem(item);
    }

    private static boolean isSulfuras(Item item) {
        return item.name.equals(SULFURAS);
    }

    private static boolean isConjuredItem(Item item) {
        return item.name.startsWith(CONJURED);
    }

    private static boolean isBackstagePasses(Item item) {
        return item.name.startsWith(BACKSTAGE_PASSES);
    }

    private static boolean isAgedBrie(Item item) {
        return AGED_BRIE.equals(item.name);
    }

}
