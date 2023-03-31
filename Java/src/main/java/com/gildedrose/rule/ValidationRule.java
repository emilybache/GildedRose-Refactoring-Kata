package com.gildedrose.rule;

import com.gildedrose.Item;

public final class ValidationRule {

	public static boolean isWithInLimit(int limit, int value) {
		return value < limit;
	}

	public static boolean isExpired(Item item) {
		return item.sellIn < 0;
	}

	public static boolean hasMinimumRequiredQuality(Item item) {
		return item.quality > 0;
	}

	private ValidationRule() {
	}

}
