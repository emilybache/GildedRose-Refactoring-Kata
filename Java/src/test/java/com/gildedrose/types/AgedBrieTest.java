package com.gildedrose.types;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.gildedrose.Item;
import com.gildedrose.enums.ProductType;

public class AgedBrieTest {
	
	@Test
	public void testAgedBrieUpdateQuality() {
		Item item = new Item(ProductType.AGED_BRIE.name(), 2, 3);
		AgedBrie agedBrie = new AgedBrie();
		agedBrie.updateQuality(item);
		assertEquals(item.quality, 4);
		assertEquals(item.sellIn, 1);
	}

}
