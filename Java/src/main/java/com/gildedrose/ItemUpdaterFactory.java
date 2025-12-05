package com.gildedrose;

public class ItemUpdaterFactory {
    public static ItemUpdater getItemUpdater(Item item) {
        if (item.name.contains("Aged Brie"))
            return new AgedBrieUpdater();

        if (item.name.contains("Sulfuras"))
            return new SulfurasUpdater();

        if (item.name.contains("Backstage"))
            return new BackstagePassUpdater();

        if (item.name.contains("Conjured"))
            return new ConjuredItemUpdater();

        return new NormalItemUpdater();
    }
}
