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
				int qualityOffset = determineEnhancingQualityOffset(item);
				item.quality = Integer.min(item.quality + qualityOffset, MAX_QUALITY);
			} else {
				int qualityOffset = 1;
				if(item.sellIn <= 0) {
					qualityOffset +=1;
				}
				item.quality = Integer.max(MIN_QUALITY, item.quality - qualityOffset);
			}

			item.sellIn -= 1;
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
		return item.name.equals(GildedRose.AGED_BRIE) || item.name.equals(BACKSTAGE_PASSES);
	}
	
	/**
	 * @param item
	 * @return
	 */
	private int determineEnhancingQualityOffset(Item item) {
		int qualityOffset = 1;
		if (item.name.equals(BACKSTAGE_PASSES)) {
			if (isExperingSale(item)) {
				qualityOffset += 1;
			}

			if (isUrgentSale(item)) {
				qualityOffset += 1;
			}
		}
		if (item.sellIn <= 0) {
			if( item.name.equals(AGED_BRIE)) {
				qualityOffset += 1;
			} else {
				qualityOffset = -item.quality;
			}
		}
		return qualityOffset;
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
}