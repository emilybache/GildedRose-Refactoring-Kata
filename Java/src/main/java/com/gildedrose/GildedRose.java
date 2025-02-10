package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            boolean isAgedBrie = item.getName().equals("Aged Brie");
            boolean isSulfuras = item.getName().equals("Sulfuras, Hand of Ragnaros");
            boolean passesToTafkalConcert = item.getName().equals("Backstage passes to a TAFKAL80ETC concert");

            if (!isAgedBrie && !passesToTafkalConcert) {
                if (item.quality > 0 && !isSulfuras) {
                    item.deductOneFromQuality();
                }
            } else if(item.quality < 50) {
                item.addOneToQuality();

                if (passesToTafkalConcert) {
                    if (item.sellIn < 11 && item.quality < 50) {
                        item.addOneToQuality();
                    }

                    if (item.sellIn < 6 && item.quality < 50) {
                        item.addOneToQuality();
                    }
                }
            }

            if (!isSulfuras) {
                item.deductSellIn();
            }

            if (item.sellIn < 0) {
                if (!isAgedBrie) {
                    if (!passesToTafkalConcert) {
                        if (item.quality > 0 && !isSulfuras) {
                            item.deductOneFromQuality();
                        }
                    } else {
                        item.setQualityToZero();
                    }
                } else if ((item.quality < 50)) {
                    item.addOneToQuality();
                }
            }
        }
    }
}
