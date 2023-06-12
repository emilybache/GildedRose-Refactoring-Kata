package com.gildedrose.types;

import com.gildedrose.Item;
import com.gildedrose.enums.CountType;
import com.gildedrose.generic.ItemType;

/***
 * "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
 * @author VIJAY G
 *
 */
public class Backstage extends ItemType<Backstage> {
	
	public Backstage() {
		super(true, CountType.MINUS, CountType.PLUS);
	}
	
	public boolean isNeutralized(Item item) {
		return (item.sellIn < 0) ? true : false;
	}

}
