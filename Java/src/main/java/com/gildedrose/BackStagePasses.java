package com.gildedrose;

public class BackStagePasses implements Goods {
	
	private static final int MAX_ALLOWED_QUALITY = 50;
	private static final int SELL_IN_MAX_THRESHOLD_DAY = 11;
	private static final int SELL_IN_MIN_THRESHOLD_DAY = 6;

	@Override
	public void updateQuality(Item item) {
		addQualityWhenWithInLimit(item);
		if (item.sellIn < SELL_IN_MAX_THRESHOLD_DAY) {
			addQualityWhenWithInLimit(item);
		}
		if (item.sellIn < SELL_IN_MIN_THRESHOLD_DAY) {
			addQualityWhenWithInLimit(item);
		}
	}

	private void addQualityWhenWithInLimit(Item item) {
		if (item.quality < MAX_ALLOWED_QUALITY) {
			item.quality++;
		}
	}
}
