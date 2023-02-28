package com.gildedrose;

import com.gildedrose.model.Goods;

import static com.gildedrose.Constants.MIN_QUALITY;

class GildedRose {

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {

        for (Integer i = 0; i < items.length; i++) {
            Goods goods = updateQuality(items[i]);
            items[i] = goods; //TODO: remove mutation
        }
    }

    private Goods updateQuality(Item item) {
        Goods goods = GoodsFactory.getGoods(item.name, item.sellIn, item.quality);
        goods.updateQuality();
        return goods;
    }
}
