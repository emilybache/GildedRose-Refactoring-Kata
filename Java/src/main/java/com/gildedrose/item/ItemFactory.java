package com.gildedrose.item;

public class ItemFactory {

    public Item createItem(String itemName, int sellIn, int quality) {
        if (itemName.equals("Sulfuras, Hand of Ragnaros")) {
            return new Sulfuras(itemName, sellIn, quality);
        } else if (itemName.equals("Aged Brie")) {
            return new AgedBrie(itemName, sellIn, quality);
        } else if (itemName.equals("Backstage passes to a TAFKAL80ETC concert")) {
            return new BackstagePassesItem(itemName, sellIn, quality);
        } else {
            return new StandardItem(itemName, sellIn, quality);
        }
    }
}
