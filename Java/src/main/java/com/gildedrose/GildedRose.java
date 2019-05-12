package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    private void increaseQuality(Item item) {
        if (item.quality < 50) {
            item.quality += 1;
        }
    }

    private void decreaseQuality(Item item) {
        if (item.quality > 0) {
            item.quality -= 1;
        }
    }

    public void updateQuality() {
        String SULFURA = "Sulfuras, Hand of Ragnaros";
        String AGED_BRIE = "Aged Brie";
        String BACKSTAGE = "Backstage passes to a TAFKAL80ETC concert";


        for (Item item : items) {
            if (item.name.equals(SULFURA)) {
                continue;
            }

            if (item.name.equals(AGED_BRIE) || item.name.equals(BACKSTAGE)) {
                increaseQuality(item);
                if (item.name.equals(BACKSTAGE)) {
                    if (item.sellIn < 11) {
                        increaseQuality(item);
                    }
                    if (item.sellIn < 6) {
                        increaseQuality(item);
                    }
                }
            } else {
                decreaseQuality(item);
            }

            item.sellIn = item.sellIn - 1;


            if (item.sellIn < 0) {
                if (item.name.equals(AGED_BRIE)) {
                    increaseQuality(item);
                } else if (item.name.equals(BACKSTAGE)) {
                    item.quality -= item.quality;
                } else {
                    decreaseQuality(item);
                }

            }
        }
    }
}
