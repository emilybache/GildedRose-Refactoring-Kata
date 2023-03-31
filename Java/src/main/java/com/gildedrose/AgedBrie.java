package com.gildedrose;

public class AgedBrie implements Goods {
	private static final int MAX_ALLOWED_QUALITY = 50;

	@Override
	public void updateQuality(Item item) {
		addQualityWhenWithInLimit(item);

	}
	
	@Override
	public void updateQualityForExpiredItem(Item item) {
		if (item.sellIn < 0) {
			addQualityWhenWithInLimit(item);
		}

	}

	private void addQualityWhenWithInLimit(Item item) {
		if (item.quality < MAX_ALLOWED_QUALITY) {
			item.quality++;
		}
	}
}
