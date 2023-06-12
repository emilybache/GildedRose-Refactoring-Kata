package com.gildedrose.generic;

import com.gildedrose.Item;

/***
 * EventHandler helps to upgrade/degrade the items
 * @author VIJAY G
 *
 */
public interface EventHandler {

	default void increaseQualityByValue(Item item, Integer value) {
		item.quality += value;
	}

	default void decreaseQualityByValue(Item item, Integer value) {
		item.quality -= value;
	}

	default void increaseSellInByValue(Item item, Integer value) {
		item.sellIn += value;
	}

	default void decreaseSellInByValue(Item item, Integer value) {
		item.sellIn -= value;
	}
	
	default void neutralizeQuality(Item item) {
		item.quality = 0;
	}

}
