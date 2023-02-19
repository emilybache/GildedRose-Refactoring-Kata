package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            if ("Sulfuras, Hand of Ragnaros".equals(items[i].name)) {
                continue;
            } else if ("Conjured Mana Cake".equals(items[i].name)) {
                while (items[i].quality > 0) {
                    if (items[i].sellIn >= 0) {
                        items[i].quality = items[i].quality - 2;
                    } else {
                        items[i].quality = items[i].quality - 4;
                    }
                }
                items[i].sellIn = items[i].sellIn - 1;
                continue;
            } else if ("Backstage passes to a TAFKAL80ETC concert".equals(items[i].name) | "Aged Brie".equals(items[i].name)) {
                while (items[i].quality < 50) {
                    if ("Backstage passes to a TAFKAL80ETC concert".equals(items[i].name) & items[i].sellIn <= 5) {
                        items[i].quality = items[i].quality + 3;
                    } else if ("Backstage passes to a TAFKAL80ETC concert".equals(items[i].name) & items[i].sellIn < 10) {
                        items[i].quality = items[i].quality + 2;
                    } else {
                        items[i].quality = items[i].quality + 1;
                    }
                }
                items[i].sellIn = items[i].sellIn - 1;
                continue;
            } else {
                // last case
                while (items[i].quality > 0) {
                    if (items[i].sellIn >= 0) {
                        items[i].quality = items[i].quality - 2;
                    } else {
                        items[i].quality = items[i].quality - 4;
                    }
                }
                items[i].sellIn = items[i].sellIn - 1;
            }
        }
    }
}
