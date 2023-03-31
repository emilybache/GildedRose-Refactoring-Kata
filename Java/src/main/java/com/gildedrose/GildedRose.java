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

            updateQualityForExpiredItem(item);
        }
    }


	private void addQualityWhenWithInLimit(Item item) {
		if (item.quality < 50) {
		    item.quality = item.quality + 1;
		}
	}

	private void updateQualityForExpiredItem(Item item) {
		if (item.sellIn < 0) {
		    if (!item.name.equals("Aged Brie")) {
		        if (!item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
		            if (item.quality > 0) {
		                if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
		                    updateQualityForExpiredItemNormal(item);
		                }
		            }
		        } else {
		            updateQualityForExpiredItemBackstagePasses(item);
		        }
		    } else {
		        addQualityWhenWithInLimit(item);
		    }
		}
	}

	private void updateQualityForExpiredItemBackstagePasses(Item item) {
		item.quality = 0;
	}

	private void updateQualityForExpiredItemNormal(Item item) {
		item.quality--;
	}
    
}