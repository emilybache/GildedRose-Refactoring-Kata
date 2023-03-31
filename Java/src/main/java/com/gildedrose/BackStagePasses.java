package com.gildedrose;

import static com.gildedrose.rule.ValidationRule.*;

public class BackStagePasses implements Goods {
	
	private static final int MAX_ALLOWED_QUALITY = 50;
	private static final int SELL_IN_MAX_THRESHOLD_DAY = 11;
	private static final int SELL_IN_MIN_THRESHOLD_DAY = 6;
	private static final int ZERO = 0;
	private static final int DEFAULT_QUALITY_TO_ADD = 1;

	@Override
	public void updateQuality(Item item) {
		int qualityToAdd = DEFAULT_QUALITY_TO_ADD;

		if (isWithInLimit(SELL_IN_MAX_THRESHOLD_DAY, item.sellIn)) {
			qualityToAdd++;
		}
		if (isWithInLimit(SELL_IN_MIN_THRESHOLD_DAY, item.sellIn)) {
			qualityToAdd++;
		}
		Goods.super.addQualityWhenWithInLimit(item, MAX_ALLOWED_QUALITY, qualityToAdd);
	}

	@Override
	public void updateQualityForExpiredItem(Item item) {
		if (isExpired(item)) {
			item.quality = ZERO;
		}
	}
}
