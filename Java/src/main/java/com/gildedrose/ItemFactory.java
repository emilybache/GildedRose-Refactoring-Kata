package com.gildedrose;

import static com.gildedrose.ItemType.fromName;

public class ItemFactory {
    public static Item createItem(String name, int sellIn, int quality) {
        var itemType = fromName(name);

        return switch (itemType) {
            case AgedBrie -> new AgedBrieItem(name, sellIn, quality);
            case BackstagePass -> new BackstagePassItem(name, sellIn, quality);
            case Sulfuras -> new SulfurasItem(name, sellIn, quality);
            case Normal -> new NormalItem(name, sellIn, quality);
        };
    }
}
