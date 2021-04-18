package com.gildedrose;

public class AgedBrie extends Item {
	
	
	
	public AgedBrie(String name, int sellIn, int quality) {
		super(name, sellIn, quality);
	}

	public Item updateQuality(Item item) {
        int newQuality = sellByDayPassed(item) ? item.quality + 2 : item.quality + 1;

        if(isQualityGreaterThanFifty(newQuality)) {
            newQuality = 50;
        }

        return new AgedBrie(item.name, item.sellIn - 1, newQuality);
    }

    private boolean sellByDayPassed(Item item) {
        return item.sellIn < 1;
    }

    private boolean isQualityGreaterThanFifty(int newQuality) {
        return newQuality > 50;
    }
}
