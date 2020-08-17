package com.gildedrose;

public class NormalItem extends Item {

	public NormalItem(String name, int sellIn, int quality) {
		super(name, sellIn, quality);
	}

	@Override
	public void update() {
		this.sellIn = this.sellIn - 1;

		this.decreaseQuality();
		if (this.sellIn < 0) {
			this.decreaseQuality();
		}
	}

}
