package com.gildedrose;

public class AgedBrieItem extends Item {

	public AgedBrieItem(String name, int sellIn, int quality) {
		super(name, sellIn, quality);
	}

	@Override
	public void update() {
		this.sellIn = this.sellIn - 1;

		this.increaseQuality();

		if (this.sellIn < 0) {
			this.increaseQuality();
		}
	}

}
