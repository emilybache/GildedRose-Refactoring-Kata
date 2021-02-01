package com.gildedrose;

class GildedRose {
	Item[] items;

	public GildedRose(Item[] items) {
		this.items = items;
	}

	private static final String AGED_BRIE_STRING = "Aged Brie";
	private static final String BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert";
	private static final String SULFRAS = "Sulfuras, Hand of Ragnaros";
	private static final String CONJURED_ITEM = "Conjured Mana Cake";
	private static final int UPPER_LIMIT_QUALITY = 50;

	/**
	 * TODO :: 1.Idea is to replace if else with separate logic depending on the
	 * type of Item we receive we can have specific methods(We can use Factory
	 * design Template for this) to handle each scenario like below methods
	 * backStagePassProcess , ConjuredItemProcess,MiscItemProcess etc.
	 * 
	 * 2. Can make this update class as static util class but rather go with factory
	 * pattern to update. Also make ENUMS for Strings.
	 */
	public void updateQuality() {
		for (int i = 0; i < items.length; i++) {
			if (!items[i].name.equalsIgnoreCase(AGED_BRIE_STRING) && !items[i].name.equalsIgnoreCase(BACKSTAGE_PASS)) {
				if (items[i].quality > 0) {
					if (!items[i].name.equalsIgnoreCase(SULFRAS)) {
						items[i].quality = items[i].quality - 1;
						if (items[i].name.equalsIgnoreCase(CONJURED_ITEM))
							items[i].quality = items[i].quality - 1;

					}
				}
			} else {
				if (items[i].quality < UPPER_LIMIT_QUALITY) {
					items[i].quality = items[i].quality + 1;

					if (items[i].name.equalsIgnoreCase(BACKSTAGE_PASS)) {
						if (items[i].sellIn < 11) {
							if (items[i].quality < UPPER_LIMIT_QUALITY) {
								items[i].quality = items[i].quality + 1;
							}
						}

						if (items[i].sellIn < 6) {
							if (items[i].quality < UPPER_LIMIT_QUALITY) {
								items[i].quality = items[i].quality + 1;
							}
						}
					}
				}
			}

			if (!items[i].name.equalsIgnoreCase(SULFRAS)) {

				items[i].sellIn = items[i].sellIn - 1;
			}

			if (items[i].sellIn < 0) {
				if (!items[i].name.equalsIgnoreCase(AGED_BRIE_STRING)) {
					if (!items[i].name.equalsIgnoreCase(BACKSTAGE_PASS)) {
						if (items[i].quality > 0) {
							if (!items[i].name.equalsIgnoreCase(SULFRAS)) {
								items[i].quality = items[i].quality - 1;
								if (items[i].name.equalsIgnoreCase(CONJURED_ITEM))
									items[i].quality = items[i].quality - 1;
							}
						}
					} else {
						items[i].quality = items[i].quality - items[i].quality;
					}
				} else {
					if (items[i].quality < 50) {
						items[i].quality = items[i].quality + 1;
					}
				}
			}
		}
	}
}