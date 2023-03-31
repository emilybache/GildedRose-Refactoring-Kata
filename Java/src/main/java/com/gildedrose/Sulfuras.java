package com.gildedrose;

public class Sulfuras implements Goods {

	@Override
	public void updateQuality(Item item) {
		// Do nothing because Sulfuras being a legendary item, never has to be sold or
		// decreases in quality.

	}

	@Override
	public void updateSellInDays(Item item) {
		// Do nothing because Sulfuras being a legendary item, never has to be sold or
		// decreases in quality.
	}

	@Override
	public void updateQualityForExpiredItem(Item item) {
		// Do nothing because Sulfuras being a legendary item, never has to be sold or
		// decreases in quality.		
	}

}
