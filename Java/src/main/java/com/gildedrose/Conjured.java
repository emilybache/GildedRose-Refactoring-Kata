package com.gildedrose;

import static com.gildedrose.rule.ValidationRule.*;

public class Conjured implements Goods {

	@Override
	public void updateQuality(Item item) {
		reduceQualityTwice(item);
	}

	@Override
	public void updateQualityForExpiredItem(Item item) {
		if (isExpired(item)) {
			reduceQualityTwice(item);
		}
	}

	private void reduceQualityTwice(Item item) {
		if (hasMinimumRequiredQuality(item)) {
			item.quality = Math.max(0, item.quality - 2);
		}
	}

}
