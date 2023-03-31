package com.gildedrose;

public class Generic implements Goods {
	@Override
	public void updateQuality(Item item) {
		if (item.quality > 0) {
			item.quality--;
		}
	}
}
