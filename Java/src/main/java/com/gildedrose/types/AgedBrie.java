package com.gildedrose.types;

import com.gildedrose.Item;
import com.gildedrose.enums.CountType;
import com.gildedrose.generic.ItemType;

/***
 * "Aged Brie" actually increases in Quality the older it gets
 * the Quality of an item is never more than 50
 * @author VIJAY G
 *
 */
public class AgedBrie extends ItemType<AgedBrie> {
	
	public AgedBrie() {
		super(false, CountType.MINUS, CountType.PLUS);
	}
	
	public int getIncrementor(Item item) {
		return (item.sellIn <= 0) ? 2 : INCREMENTOR;
	}

}
