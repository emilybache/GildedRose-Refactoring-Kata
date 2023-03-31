package com.gildedrose;

import static com.gildedrose.rule.ValidationRule.*;

public class Generic implements Goods {
	
	@Override
	public void updateQuality(Item item) {
		if (hasMinimumRequiredQuality(item)) {
			item.quality--;
		}
	}

	@Override
	public void updateQualityForExpiredItem(Item item) {
		if (isExpired(item) && hasMinimumRequiredQuality(item)) {
			item.quality--;
		}
	}
}
