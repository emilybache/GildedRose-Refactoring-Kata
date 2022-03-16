package com.gildedrose;

class GildedRose {
	Item[] items;

	public static final String AGED_BRIE = "Aged Brie";
	public static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
	public static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
	public static final String CONJURED = "Conjured";

	public GildedRose(Item[] items) {
		this.items = items;
	}

	public void updateQuality() {
		for (Item item : items) {
			updateQuality(item);
			updateSellIn(item);
		}
	}

	private void updateQuality(Item item) {

		switch (item.name) {

		case AGED_BRIE:
			updateAgedBrie(item);
			break;
		case BACKSTAGE_PASSES:
			updateBackStagePasses(item);
			break;
		case SULFURAS:
			updateSulfras(item);
			break;
		case CONJURED:
			updateConjured(item);
			break;
		default:
			updateNormalItem(item);
			break;

		}

	}

	private void updateAgedBrie(Item item) {
		increaseQualityByOne(item);
		if (isItemExpired(item)) {
			increaseQualityByOne(item);
		}
	}

	private void updateBackStagePasses(Item item) {

		if (!isItemExpired(item)) {
			increaseQualityByOne(item);

			if (item.sellIn < 11) {
				increaseQualityByOne(item);
			}
			if (item.sellIn < 6) {
				increaseQualityByOne(item);
			}
		} else {
			item.quality = 0;
		}

	}

	private void updateSulfras(Item item) {
		// todo...
	}

	private void updateConjured(Item item) {
		updateNormalItem(item);
		updateNormalItem(item);
	}

	private void updateNormalItem(Item item) {

		decreaseQualityByOne(item);
		if (isItemExpired(item)) {
			decreaseQualityByOne(item);
		}

	}
	private void updateSellIn(Item item) {

		if (!item.name.equals(SULFURAS)) {
			item.sellIn = item.sellIn - 1;
		}

	}
	
	private boolean isItemExpired(Item item) {
		return item.sellIn < 0;

	}

	public void decreaseQualityByOne(Item item) {
		if (item.quality > 0) {
			item.quality = item.quality - 1;
		}

	}

	public void increaseQualityByOne(Item item) {
		if (item.quality < 50) {
			item.quality = item.quality + 1;
		}

	}

}
