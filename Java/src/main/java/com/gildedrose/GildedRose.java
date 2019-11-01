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
            item.updateSellIn();
            if(item.name.equals(Brie.BRIE)){
                updateBrieQuality(item);
            }else if (item.name.equals(Backstage.BACKSTAGE)){
                updateBackstageQuality(item);
            }else if (!item.name.equals(Sulfuras.SULFURAS)){
                updateItemQuality(item);
            }
        }
    }

    private void updateItemQuality(Item item) {
        if (!item.name.equals(Backstage.BACKSTAGE)) {
            if (item.quality > 0 ) {
                item.decreaseQuality();
            }
        }
        else {
            if (item.quality < 50) {
                item.increaseQuality();
            }
        }

        if (item.sellIn < 0) {
            if (!item.name.equals(Backstage.BACKSTAGE)) {
                if ( item.quality > 0) {
                    item.decreaseQuality();
                }
            } else {
                item.quality = 0;
            }
        }
    }

    private void updateBackstageQuality(Item item) {
        if (item.quality < 50) {
            item.increaseQuality();
        }
        if (item.sellIn < 0) {
            item.quality = 0;
        }
    }

    private void updateBrieQuality(Item item) {
        if (item.quality < 50) {
            item.increaseQuality();
        }
        if (item.sellIn < 0) {
            if (item.quality < 50) {
                item.increaseQuality();
            }
        }
    }


}