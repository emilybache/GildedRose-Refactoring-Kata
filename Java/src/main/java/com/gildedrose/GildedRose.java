package com.gildedrose;

class GildedRose {
	
    private static final int MIN_QUALITY = 0;
	private static final int MAX_QUALITY = 50;
	public static final String AGED_BRIE = "Aged Brie";
	public static final String SULFURAS_HAND_OF_RAGNAROS = "Sulfuras, Hand of Ragnaros";
	public static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
	public static final String CONJURED_MANA_CAKE = "Conjured Mana Cake";
    
	Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
		for (int i = 0; i < items.length; i++) {
			Item item = items[i];
			if(isLegendaryItem(item.name)) {
				continue;
			}
			// update quality
			if (isEnhancingItem(item.name)) {
				int qualityOffset = determineEnhancingQualityOffset(item);
				item.quality = Integer.min(item.quality + qualityOffset, MAX_QUALITY);
			} else {
				int qualityOffset = determineDegradingQualityOffset(item);
				item.quality = Integer.max(MIN_QUALITY, item.quality - qualityOffset);
			}
			
			// update sell in value
			item.sellIn -= 1;
		}
    }

	/**
	 * @param name
	 * @return
	 */
	private boolean isLegendaryItem(String name) {
		return name.equals(SULFURAS_HAND_OF_RAGNAROS);
	}

	/**
	 * @param name
	 * @return
	 */
	private boolean isEnhancingItem(String name) {
		return name.equals(GildedRose.AGED_BRIE) || name.equals(BACKSTAGE_PASSES);
	}
	
	/**
	 * @param name
	 * @return
	 */
	private boolean isConjuredItem(String name) {
		return name.equals(CONJURED_MANA_CAKE);
	}
	
	/**
	 * @param item
	 * @return
	 */
	private int determineEnhancingQualityOffset(Item item) {
		int qualityOffset = 1;
		if (item.name.equals(BACKSTAGE_PASSES)) {
			if (isExpiredSale(item.sellIn)) {
				qualityOffset = -item.quality;
			} else if (isUrgentSale(item.sellIn)) {
				qualityOffset += 2;
			} else if (isExperingSale(item.sellIn)) {
				qualityOffset += 1;
			}
		} else if (isExpiredSale(item.sellIn)) {
			qualityOffset *= 2;
		}
		return qualityOffset;
	}
	
	/**
	 * @param item
	 * @return
	 */
	private int determineDegradingQualityOffset(Item item) {
		int qualityOffset = 1;
		if(item.sellIn <= 0) {
			qualityOffset *=2;
		}
		if(isConjuredItem(item.name)) {
			qualityOffset *= 2;
		}
		return qualityOffset;
	}

	/**
	 * @param sellIn
	 * @return
	 */
	private boolean isExpiredSale(int sellIn) {
		return sellIn <= 0;
	}

	/**
	 * @param sellIn
	 * @return
	 */
	private boolean isUrgentSale(int sellIn) {
		return sellIn < 6;
	}

	/**
	 * @param sellIn
	 * @return
	 */
	private boolean isExperingSale(int sellIn) {
		return sellIn < 11;
	}
}