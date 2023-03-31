package com.gildedrose;

import com.gildedrose.factory.GoodsFactory;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

	public void updateQuality() {
		for (Item item : items) {
			Goods goods = GoodsFactory.getGoods(item.name);
			goods.updateQuality(item);
			goods.updateSellInDays(item);
			goods.updateQualityForExpiredItem(item);
		}
	}

}