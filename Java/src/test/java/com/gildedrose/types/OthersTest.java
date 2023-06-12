package com.gildedrose.types;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.gildedrose.Item;

public class OthersTest {
	
	@Test
	public void testOthersUpdateQuality() {
		Item item = new Item("Other item", 2, 3);
		Others others = new Others();
		others.updateQuality(item);
		assertEquals(item.quality, 2);
		assertEquals(item.sellIn, 1);
	}

}
