package com.gildedrose;

class GildedRose {
	
    private static final int MIN_QUALITY = 0;
	private static final int MAX_QUALITY = 50;
	public static final String AGED_BRIE = "Aged Brie";
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
			if (isEnhancingItem(item)) {
				enhanceItem(item);
			} else {
				degradeItem(item);
			}

			updateSellInValue(item);

			if (item.sellIn < 0) {
				if (!item.name.equals(AGED_BRIE)) {
					if (!item.name.equals(BACKSTAGE_PASSES)) {
						degradeItem(item);
					} else {
						item.quality = MIN_QUALITY;
					}
				} else {
					item.quality = Integer.min(item.quality + 1, MAX_QUALITY);
				}
			}
		}
    }

	/**
	 * @param item
	 */
	private void degradeItem(Item item) {
		item.quality = Integer.max(MIN_QUALITY, item.quality - 1);
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
		return item.name.equals(GildedRose.AGED_BRIE) || item.name.equals(BACKSTAGE_PASSES);
	}
	
	/**
	 * @param item
	 */
	private void enhanceItem(Item item) {
		if (item.name.equals(BACKSTAGE_PASSES)) {
			if (isExperingSale(item)) {
				item.quality = item.quality + 1;
			}

			if (isUrgentSale(item)) {
				item.quality = item.quality + 1;
			}
		}
		item.quality = Integer.min(item.quality + 1, MAX_QUALITY);
	}

	/**
	 * @param item
	 * @return
	 */
	private boolean isUrgentSale(Item item) {
		return item.sellIn < 6;
	}

	/**
	 * @param item
	 * @return
	 */
	private boolean isExperingSale(Item item) {
		return item.sellIn < 11;
	}

	/**
	 * @param item the item of which the sell in value is updated 
	 */
	private void updateSellInValue(Item item) {
		item.sellIn = item.sellIn - 1;
	}
}