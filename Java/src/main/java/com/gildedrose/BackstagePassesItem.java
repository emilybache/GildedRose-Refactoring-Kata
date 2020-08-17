package com.gildedrose;

public class BackstagePassesItem extends Item {

	public BackstagePassesItem(String name, int sellIn, int quality) {
		super(name, sellIn, quality);
	}

	@Override
	public void update() {
		this.sellIn = this.sellIn - 1;

		this.increaseQuality();

		if (this.sellIn < 10) {
			this.increaseQuality();
		}

		if (this.sellIn < 5) {
			this.increaseQuality();
		}

		if (this.sellIn < 0) {
			this.quality = this.quality - this.quality;
		}
	}

}
