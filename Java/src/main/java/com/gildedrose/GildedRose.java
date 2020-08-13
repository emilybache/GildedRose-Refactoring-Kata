package com.gildedrose;

class GildedRose {
	Item[] items;

	public GildedRose(Item[] items) {
		this.items = items;
	}

	public void updateQuality() {
		for (Item item : items) {
			updateItem(item);
		}
	}

	private void updateItem(Item item) {
		if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
			return;
		}

		if (item.name.equals("Aged Brie")) {
			if (item.quality < 50) {
				item.quality = item.quality + 1;
			}
		} else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
			if (item.quality < 50) {
				item.quality = item.quality + 1;

				if (item.sellIn < 11) {
					if (item.quality < 50) {
						item.quality = item.quality + 1;
					}
				}

				if (item.sellIn < 6) {
					if (item.quality < 50) {
						item.quality = item.quality + 1;
					}
				}
			}
		} else if (item.quality > 0) {
			item.quality = item.quality - 1;
		}

		item.sellIn = item.sellIn - 1;

		if (item.sellIn < 0) {
			if (item.name.equals("Aged Brie")) {
				if (item.quality < 50) {
					item.quality = item.quality + 1;
				}
			} else {
				if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
					item.quality = item.quality - item.quality;
				} else {
					if (item.quality > 0) {
						item.quality = item.quality - 1;
					}
				}
			}
		}
	}
}
