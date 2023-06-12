package com.gildedrose.types;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.gildedrose.Item;
import com.gildedrose.enums.ProductType;

public class ConjuredTest {
	
	@Test
	public void testConjuredUpdateQuality() {
		Item item = new Item(ProductType.CONJURED.name(), 2, 3);
		Conjured conjured = new Conjured();
		conjured.updateQuality(item);
		assertEquals(item.quality, 1);
		assertEquals(item.sellIn, 1);
	}

}
