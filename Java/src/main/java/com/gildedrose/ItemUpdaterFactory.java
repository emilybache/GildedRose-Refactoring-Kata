package com.gildedrose;

public final class ItemUpdaterFactory {

    private ItemUpdaterFactory() {
        throw new UnsupportedOperationException("Class cannot be instantiated");
    }

    public static ItemUpdater getUpdater(Item item) {
        switch (item.name) {
            case "Aged Brie":
                return new AgedBrieItemUpdater();
            case "Backstage passes to a TAFKAL80ETC concert":
                return new BackstagePassesItemUpdater();
            case "Sulfuras, Hand of Ragnaros":
                return new SulfurasItemUpdater();
            case "Conjured Mana Cake":
                return new ConjuredItemUpdater();
            default:
                return new RegularItemUpdater();
        }
    }
}
