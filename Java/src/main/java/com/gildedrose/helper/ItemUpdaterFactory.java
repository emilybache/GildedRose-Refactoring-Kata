package com.gildedrose.helper;

import com.gildedrose.GildedRose;
import com.gildedrose.Item;

public class ItemUpdaterFactory {
    public static ItemAgentService getUpdater(Item item) {
        switch (item.name) {
            case "Aged Brie":
                return new GildedRose.AgedBrieUpdater();
            case "Backstage passes to a TAFKAL80ETC concert":
                return new GildedRose.BackstagePassUpdater();
            case "Sulfuras, Hand of Ragnaros":
                return new GildedRose.SulfurasUpdater();
            case "Conjured Mana Cake":
                return new GildedRose.ConjuredItemUpdater();
            default:
                return new GildedRose.NormalItemUpdater();
        }}
}
