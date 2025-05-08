package com.gildedrose.updaters;

import com.gildedrose.Item;

import static com.gildedrose.GildedRoseConstants.*;

public class ItemUpdaterFactory {
    public static ItemUpdater getUpdater(Item item) {
        String name = item.name;

        if (name.equals(AGED_BRIE)) {
            return new AgedBrieUpdater(item);
        } else if (name.equals(SULFURAS)) {
            return new SulfurasUpdater(item);
        } else if (name.equals(BACKSTAGE_PASS)) {
            return new BackstagePassUpdater(item);
        } else if (name.toLowerCase().contains(CONJURED)) {
            return new ConjuredItemUpdater(item);
        } else {
            return new NormalItemUpdater(item);
        }
    }
}
