package com.gildedrose;

public class Generic implements Goods {
	@Override
	public void updateQuality(Item item) {
		if (item.quality > 0) {
			item.quality--;
		}
	}

	@Override
	public void updateQualityForExpiredItem(Item item) {
		if (item.sellIn < 0 && item.quality > 0) {
			item.quality--;
		}
	}
}
