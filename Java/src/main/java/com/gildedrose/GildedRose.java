package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    {
        for (int i = 0; i < items.length; i++) {
            Item item = items[i];

            if (item.name.equals("Sulfuras, Hand of Ragnaros"))continue;

            if (item.name.equals("Aged Brie") || item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                if (item.quality < 50) {
                    item.quality += 1;

                    if (item.sellIn < 11) {
                        if (item.quality < 50) {
                            item.quality += 1;
                        }
                    }

                    if (item.sellIn < 6) {
                        if (item.quality < 50) {
                            item.quality += 1;
                        }
                    }
                }
            } else {
                if (item.quality > 0) {
                    item.quality -= 1;
                }
            }

            item.sellIn -= 1;

            if (item.sellIn > 0) continue;

            if (!item.name.equals("Aged Brie") || !item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                if (item.quality < 0) continue;

                item.quality += 1;
                continue;
            }

            if (item.name.equals("Aged Brie")) {
                if (item.quality > 50) continue;

                item.quality += 1;
            } else {
                item.quality = 0;
            }
        }
    }
}
