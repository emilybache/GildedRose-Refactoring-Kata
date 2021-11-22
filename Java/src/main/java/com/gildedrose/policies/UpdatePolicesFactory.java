package com.gildedrose.policies;

import com.gildedrose.Item;

 class UpdatePolicesFactory {

     static UpdatePolicy getUpdatePolicy(Item item) {
        return switch (item.name) {
            case "Aged Brie" -> new AgedBrieUpdatePolicy(item);
            case "Backstage passes to a TAFKAL80ETC concert" -> new BackstagePassesUpdatePolicy(item);
            case "Sulfuras, Hand of Ragnaros" -> new SulfurasUpdatePolicy(item);
            case "Conjured" -> new ConjuredUpdatePolicy(item);
            default -> new UpdatePolicy(item);
        };
    }

}
