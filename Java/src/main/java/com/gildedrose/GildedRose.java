package com.gildedrose;

import com.gildedrose.item.Backstage;
import com.gildedrose.item.Brie;
import com.gildedrose.item.Item;
import com.gildedrose.item.Sulfuras;

class GildedRose {
    Item[] items;


    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {

            final Item item = items[i];

            if (!item.name.equals(Brie.BRIE) && !item.name.equals(Backstage.BACKSTAGE)) {
                if (item.quality > 0 && !item.name.equals(Sulfuras.SULFURAS)) {
                    decreaseQualityByOne(item);
                }
            }
            else {
                if (item.quality < 50) {
                    increaseQuality(item);
                }
            }

            updateSellIn(item);

            if (item.sellIn < 0) {
                if (item.name.equals(Brie.BRIE)) {
                    if (item.quality < 50) {
                        item.increaseQuality();
                    }
                } else {
                    if (!item.name.equals(Backstage.BACKSTAGE)) {
                        if (!item.name.equals(Sulfuras.SULFURAS) && item.quality > 0) {
                            decreaseQualityByOne(item);
                        }
                    } else {
                        item.quality = 0;
                    }
                }
            }
        }
    }

    private void decreaseQualityByOne(Item item) {
        item.decreaseQuality();
    }

    private void increaseQuality(Item item) {
        item.increaseQuality();
        if (item.name.equals(Backstage.BACKSTAGE)) {
            item.increaseBackstageQuality();
        }
    }

    private void updateSellIn(Item item) {
        if (!item.name.equals(Sulfuras.SULFURAS)) {
            item.sellIn = item.sellIn - 1;
        }
    }
}