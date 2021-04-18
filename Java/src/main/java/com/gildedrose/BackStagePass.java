package com.gildedrose;

public class BackStagePass extends Item {
// "Sulfuras", being a legendary item, never has to be sold or decreases in Quality	
	int newQuality = 0;
	
	public BackStagePass(String name, int sellIn, int quality) {
		super(name, sellIn, quality);
	}

	public Item updateQuality(Item item) {
		
		if (noSellin(item)) {
			newQuality = 0;
		} else if (sellin5DaysOrLess(item) ) {
			newQuality = newQuality + 3;
		} else if (sellin10DaysOrLess(item)) {
			newQuality =  newQuality + 2;
		} else {
			newQuality = newQuality + 1;
		}
		
		if (QualtyGT50(item)) {
			newQuality =  50;
		}
        return new BackStagePass(item.name, item.sellIn -  1, newQuality);
    }
	
	private boolean QualtyGT50(Item item) {
		return newQuality > 50;
	}

	private boolean sellin10DaysOrLess(Item item) {
		// TODO Auto-generated method stub
		return item.sellIn < 11;
	}

	private boolean sellin5DaysOrLess(Item item) {
		return item.sellIn < 6;
	}

	private boolean noSellin(Item item) {
		return item.sellIn < 1;
	}
 }
