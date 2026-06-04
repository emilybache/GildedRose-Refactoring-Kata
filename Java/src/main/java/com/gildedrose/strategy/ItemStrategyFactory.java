package com.gildedrose.strategy;

import com.gildedrose.Item;

public class ItemStrategyFactory {
    public static ItemStrategyUpdate getStrategy(Item item){
        switch (item.name) {
            case "Aged Brie": return new AgedBrieStrategy();
            case "Backstage passes to a TAFKAL80ETC concert": return new ConcertStrategy();
            case "Sulfuras, Hand of Ragnaros": return new SulfurasStrategy();
            default: return new DefaultItemStrategy();
        }
    }    
}
