package com.gildedrose.types;

import com.gildedrose.enums.CountType;
import com.gildedrose.generic.ItemType;

/***
 * Once the sell by date has passed, Quality degrades twice as fast
 * The Quality of an item is never negative
 * @author VIJAY G
 *
 */
public class Others extends ItemType<Others> {
	
	public Others() {
		super(false, CountType.MINUS, CountType.MINUS);
	}

}
