package com.gildedrose.item;

public class ItemFactory {

    public CustomisedItem customiseItem(Item item) {
        if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
            return new Sulfuras(item);
        } else if (item.name.equals("Aged Brie")) {
            return new AgedBrie(item);
        } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            return new BackstagePassesItem(item);
        } else {
            return new StandardItem(item);
        }
    }
}
