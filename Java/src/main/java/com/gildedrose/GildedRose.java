package com.gildedrose;

class GildedRose {
	
    public static final String SULFURAS_HAND_OF_RAGNAROS = "Sulfuras, Hand of Ragnaros";
	public static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    
	Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
		for (int i = 0; i < items.length; i++) {
			Item item = items[i];
			if(isLegendaryItem(item)) {
				continue;
			}
			if (!isEnhancingItem(item)) {
				if (item.quality > 0) {
					item.quality = item.quality - 1;
				}
			} else {
				changeQualityOfBrieAndBackstagePasses(item);
			}

			updateSellInValue(item);

			if (item.sellIn < 0) {
				if (!item.name.equals("Aged Brie")) {
					if (!item.name.equals(BACKSTAGE_PASSES)) {
						if (item.quality > 0) {
							item.quality = item.quality - 1;
						}
					} else {
						item.quality = 0;
					}
				} else if (item.quality < 50) {
					item.quality = item.quality + 1;
				}
			}
		}
    }

	/**
	 * @param item
	 * @return
	 */
	private boolean isLegendaryItem(Item item) {
		return item.name.equals(SULFURAS_HAND_OF_RAGNAROS);
	}

	/**
	 * @param item
	 * @return
	 */
	private boolean isEnhancingItem(Item item) {
		return item.name.equals("Aged Brie") || item.name.equals(BACKSTAGE_PASSES);
	}

	/**
	 * @param item
	 */
	private void changeQualityOfBrieAndBackstagePasses(Item item) {
		if (item.quality < 50) {
		    item.quality = item.quality + 1;

		    if (item.name.equals(BACKSTAGE_PASSES)) {
				if (isExperingSale(item)) {
					item.quality = item.quality + 1;
				}

				if (isUrgentSale(item)) {
					item.quality = item.quality + 1;
				}
		    }
		}
	}

	/**
	 * @param item
	 * @return
	 */
	private boolean isUrgentSale(Item item) {
		return item.sellIn < 6 && item.quality < 50;
	}

	/**
	 * @param item
	 * @return
	 */
	private boolean isExperingSale(Item item) {
		return item.sellIn < 11 && item.quality < 50;
	}

	/**
	 * @param item the item of which the sell in value is updated Only if the item
	 *             is legendary, do not update this value
	 */
	private void updateSellInValue(Item item) {
		item.sellIn = item.sellIn - 1;
	}
}