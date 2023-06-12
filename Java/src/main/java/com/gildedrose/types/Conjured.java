package com.gildedrose.types;

import com.gildedrose.Item;

/***
 * "Conjured" items degrade in Quality twice as fast as normal items
 * @author VIJAY G
 *
 */
public class Conjured extends Others {
	
	public void decreaseQualityByValue(Item item, Integer value) {
		item.quality -= 2;
	}

}
