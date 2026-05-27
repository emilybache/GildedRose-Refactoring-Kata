package com.gildedrose.experts;

import com.gildedrose.Item;
import com.gildedrose.ItemUpdater;

public class ItemUpdaterFactory {
    public static ItemUpdater createItemUpdater(Item item) {
       switch(item.name) {
           case "Aged Brie":
               return new AgedBrieUpdater();
           case "Backstage passes to a TAFKAL80ETC concert":
               return new BackstagePassUpdater();
           case "Sulfuras, Hand of Ragnaros":
               return new SulfurasUpdater();
           default:
               return new StandardUpdater();
       }
    };
}
