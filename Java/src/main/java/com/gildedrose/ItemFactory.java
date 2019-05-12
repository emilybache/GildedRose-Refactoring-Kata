package com.gildedrose;

import com.gildedrose.items.*;

public class ItemFactory {
    public static final String SULFURA = "Sulfuras, Hand of Ragnaros";
    public static final String AGED_BRIE = "Aged Brie";
    public static final String BACKSTAGE = "Backstage passes to a TAFKAL80ETC concert";
    public static final String CONJURED = "Conjured Mana Cake";


    public ItemInterface createItemType(Item item) {
        if (item.name.equals(AGED_BRIE)) {
            return new AgedBrie(item);
        } else if (item.name.equals(BACKSTAGE)) {
            return new BackStageItem(item);
        } else if (item.name.equals(CONJURED)) {
           return new ConjuredItem(item);
        } else if (item.name.equals(SULFURA)) {
            return new SulfuraItem(item);
        } else {
            return new NormalItem(item);
        }
    }

}
