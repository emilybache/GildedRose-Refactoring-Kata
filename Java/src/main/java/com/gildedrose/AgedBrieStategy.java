package com.gildedrose;

public class AgedBrieStategy implements ItemStrategy {

	@Override
	public Item updateItem(Item item) {
        int newQuality = sellByDayPassed(item) ? item.quality + 2 : item.quality + 1;

        if(isQualityGreaterThanFifty(newQuality)) {
            newQuality = 50;
        }

        return new Item(item.name, item.sellIn - 1, newQuality);
    }

    private boolean sellByDayPassed(Item item) {
        return item.sellIn < 1;
    }

    private boolean isQualityGreaterThanFifty(int newQuality) {
        return newQuality > 50;
    }
}
