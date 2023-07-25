package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {

        for (int i = 0; i < items.length; i++) {
            Item item = items[i];

            if (item.name.equals("Aged Brie")) {
                item.quality = Math.min(50, item.quality + 1);

                if (item.sellIn < 10) {
                    item.quality = Math.min(50, item.quality + 1);
                }

                if (item.sellIn < 0) {
                    item.quality = Math.min(50, item.quality + 1);
                }
            } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                item.quality = Math.min(50, item.quality + 1);

                if (item.sellIn < 11) {
                    item.quality = Math.min(50, item.quality + 2);
                }

                if (item.sellIn < 6) {
                    item.quality = Math.min(50, item.quality + 3);
                }

                if (item.sellIn < 0) {
                    item.quality = 0;
                }
            } else if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                item.quality = Math.max(0, item.quality - 1);

                if (item.sellIn < 0) {
                    item.quality = Math.max(0, item.quality - 2);
                }
            }

            item.sellIn= Math.max(0, item.sellIn -1);;
        }
    }
}
