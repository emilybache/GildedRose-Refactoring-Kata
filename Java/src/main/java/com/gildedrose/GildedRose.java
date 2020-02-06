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
				int qualityOffset = determineQualityOffset(item);
				enhanceItem(item, qualityOffset);
			} else {
				degradeItem(item,1);
			}

			updateSellInValue(item);

			if (item.sellIn < 0) {
				if (!item.name.equals(AGED_BRIE)) {
					if (!item.name.equals(BACKSTAGE_PASSES)) {
						degradeItem(item,1);
					} else {
						degradeItem(item,item.quality);
					}
				} else {
					enhanceItem(item, 1);
				}
			}
		}
    }

	/**
	 * @param item
	 */
	private void degradeItem(Item item, int qualityOffset) {
		item.quality = Integer.max(MIN_QUALITY, item.quality - qualityOffset);
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
	private void enhanceItem(Item item, int qualityOffset) {
		item.quality = Integer.min(item.quality + qualityOffset, MAX_QUALITY);
	}

	/**
	 * @param item
	 * @return
	 */
	private int determineQualityOffset(Item item) {
		int qualityOffsett = 1;
		if (item.name.equals(BACKSTAGE_PASSES)) {
			if (isExperingSale(item)) {
				qualityOffsett += 1;
			}

			if (isUrgentSale(item)) {
				qualityOffsett += 1;
			}
		}
		return qualityOffsett;
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