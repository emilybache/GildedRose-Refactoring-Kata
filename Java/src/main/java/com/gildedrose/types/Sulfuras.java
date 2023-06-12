package com.gildedrose.types;

import com.gildedrose.enums.CountType;
import com.gildedrose.generic.ItemType;

/***
 * "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
 * @author VIJAY G
 *
 */
public class Sulfuras extends ItemType<Sulfuras> {
	
	public Sulfuras() {
		super(true, CountType.NONE, CountType.NONE);
	}

}
