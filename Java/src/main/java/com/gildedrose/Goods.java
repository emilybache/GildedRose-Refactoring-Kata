package com.gildedrose;

public interface Goods {
	
	public void updateQuality(Item item);

	default void updateSellInDays(Item item) {
		item.sellIn--;
	}
}
