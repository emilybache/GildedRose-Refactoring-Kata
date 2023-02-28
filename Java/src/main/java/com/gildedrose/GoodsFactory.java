package com.gildedrose;

import com.gildedrose.model.*;

public class GoodsFactory {

    public static Goods getGoods(String name, int initialSellIn, int initialQuality) {

        switch (name) {
            case "Aged Brie":
                return new AgedBrie(initialSellIn, initialQuality);
            case "Conjured Mana Cake":
                return new Conjured(initialSellIn, initialQuality);
            case "Sulfuras, Hand of Ragnaros":
                return new Sulfuras(initialSellIn, initialQuality);
            case "Backstage passes to a TAFKAL80ETC concert":
                return new BackstagePasses(initialSellIn, initialQuality);
            default:
                return new NormalItem(name, initialSellIn, initialQuality);
        }

    }
}
