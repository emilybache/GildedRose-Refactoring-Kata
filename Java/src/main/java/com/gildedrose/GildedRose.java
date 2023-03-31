package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
		for (Item item : items) {
			if (!item.name.equals("Aged Brie")
                    && !item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                if (item.quality > 0) {
                    if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                    	updateQualityForNormalItem(item);
                    }
                }
            } else {
                if (item.quality < 50) {
                    item.quality = item.quality + 1;

                    if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                        updateQualityForBackstagePasses(item);
                    }
                }
            }

            if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
				updateSellInDays(item);
            }

            updateQualityForExpiredItem(item);
        }
    }

	private void updateQualityForBackstagePasses(Item item) {
		if (item.sellIn < 11) {
		    addQualityWhenWithInLimit(item);
		}

		if (item.sellIn < 6) {
		    addQualityWhenWithInLimit(item);
		}
	}
    
	private void updateQualityForNormalItem(Item item) {
		item.quality--;
	}

	private void addQualityWhenWithInLimit(Item item) {
		if (item.quality < 50) {
		    item.quality = item.quality + 1;
		}
	}

	private void updateQualityForExpiredItem(Item item) {
		if (item.sellIn < 0) {
		    if (!item.name.equals("Aged Brie")) {
		        if (!item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
		            if (item.quality > 0) {
		                if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
		                    item.quality = item.quality - 1;
		                }
		            }
		        } else {
		            item.quality = item.quality - item.quality;
		        }
		    } else {
		        addQualityWhenWithInLimit(item);
		    }
		}
	}
    
    
	private void updateSellInDays(Item item) {
		item.sellIn--;
	}
}