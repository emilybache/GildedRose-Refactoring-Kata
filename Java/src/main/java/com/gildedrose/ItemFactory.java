package com.gildedrose;

public class ItemFactory {
    public static ItemWrapper create(Item item) {
        switch (item.name) {
            case Constants.AgedBrie:
                return new updateBrie(item);
            case Constants.Backstage:
                return new updateBackstage(item);
            case Constants.Sulfuras:
                return new updateLegendary(item);
            case Constants.Conjured:
                 return new updateConjured(item);
            default:
                return new updateOther(item);
        }
    }
}
