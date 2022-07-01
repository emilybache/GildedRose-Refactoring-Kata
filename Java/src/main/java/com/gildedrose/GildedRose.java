package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            if (item.name.contains("Sulfuras")) {
                // legendaries don't change stats so just skip
                continue;
            } else if (item.name.contains("Aged Brie")) {
                // handling special aged brie rules
                if (item.quality < 50) {
                    if (item.sellIn <= 0) {
                        item.quality += 2;
                    } else {
                        item.quality++;
                    }
                }
            } else if (item.name.contains("Backstage passes")) {
                // handling backstage passes
                if (item.sellIn <= 0) {
                    item.quality = 0;
                } else if (item.sellIn <= 5) {
                    item.quality += 3;
                } else if (item.sellIn <= 10) {
                    item.quality += 2;
                }
            } else if (item.name.startsWith("Conjured")) {
                // conjured is twice the degrade rate as regular items
                if (item.sellIn <= 0) {
                    item.quality -= 4;
                }  else {
                    item.quality -= 2;
                }
            } else {
                // just a regular item
                if (item.sellIn <= 0) {
                    item.quality -= 2;
                }  else {
                    item.quality--;
                }
            }

            if (item.quality < 0) {
                item.quality = 0;
            }

            item.sellIn--;
        }
    }
}
