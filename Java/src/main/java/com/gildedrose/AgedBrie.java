package com.gildedrose;

import static com.gildedrose.rule.ValidationRule.isExpired;

public class AgedBrie implements Goods {
	
	private static final int MAX_ALLOWED_QUALITY = 50;

	@Override
	public void updateQuality(Item item) {
		Goods.super.addQualityWhenWithInLimit(item, MAX_ALLOWED_QUALITY);

	}

	@Override
	public void updateQualityForExpiredItem(Item item) {
		if (isExpired(item)) {
			Goods.super.addQualityWhenWithInLimit(item, MAX_ALLOWED_QUALITY);
		}
	}

}
