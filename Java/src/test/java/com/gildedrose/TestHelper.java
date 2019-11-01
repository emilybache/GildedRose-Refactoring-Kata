package com.gildedrose;

import com.gildedrose.item.Backstage;
import com.gildedrose.item.Brie;
import com.gildedrose.item.Item;
import com.gildedrose.item.Sulfuras;

public class TestHelper {

    static Item getItem(String name, Integer sellIn, Integer quality){
        switch (name){
            case "Backstage passes to a TAFKAL80ETC concert":
                return new Backstage(sellIn, quality);
            case "Aged Brie":
                return new Brie(sellIn, quality);
            case "Sulfuras, Hand of Ragnaros":
                return new Sulfuras(sellIn, quality);
            default:
                return new Item(name, sellIn, quality);
        }
    }
}
