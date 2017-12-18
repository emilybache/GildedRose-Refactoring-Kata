package com.gildedrose.item;

public class CustomisedItemFactory {

    public final static String SULFURAS = "Sulfuras, Hand of Ragnaros";
    public final static String BRIE = "Aged Brie";
    public final static String BACKSTAGE_PASSES_ITEM = "Backstage passes to a TAFKAL80ETC concert";
    public final static String CONJURED_ITEM = "Conjured";

    public CustomisedItem customiseItem(Item item) {
        if (item.name.equals(SULFURAS)) {
            return new Sulfuras();
        } else if (item.name.equals(BRIE)) {
            return new AgedBrie(item);
        } else if (item.name.equals(BACKSTAGE_PASSES_ITEM)) {
            return new BackstagePassesItem(item);
        } else if (item.name.equals(CONJURED_ITEM)) {
            return new ConjuredItem(item);
        } else {
            return new StandardItem(item);
        }
    }
}
