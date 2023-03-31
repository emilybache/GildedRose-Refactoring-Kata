package com.gildedrose;

import static com.gildedrose.rule.ValidationRule.isExpired;

public class AgedBrie implements Goods {
	
	private static final int MAX_ALLOWED_QUALITY = 50;
	
	private static final int QUALITY_TO_ADD = 1;

	@Override
	public void updateQuality(Item item) {
		Goods.super.addQualityWhenWithInLimit(item, MAX_ALLOWED_QUALITY, QUALITY_TO_ADD);
	}

	@Override
	public void updateQualityForExpiredItem(Item item) {
		if (isExpired(item)) {
			Goods.super.addQualityWhenWithInLimit(item, MAX_ALLOWED_QUALITY, QUALITY_TO_ADD);
		}
	}

}
