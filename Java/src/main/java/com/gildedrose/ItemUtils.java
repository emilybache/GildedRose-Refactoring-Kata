package com.gildedrose;

class ItemUtils {

    static boolean isAgedBrie(Item item) {
        return item.name.equals("Aged Brie");
    }

    static boolean isBackstage(Item item) {
        return item.name.equals("Backstage passes to a TAFKAL80ETC concert");
    }

    static boolean isSulfuras(Item item) {
        return item.name.equals("Sulfuras, Hand of Ragnaros");
    }

}
