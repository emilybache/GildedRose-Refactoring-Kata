package com.gildedrose.types;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.gildedrose.Item;
import com.gildedrose.enums.ProductType;

public class SulfurasTest {
	
	@Test
	public void testSulfurasUpdateQuality() {
		Item item = new Item(ProductType.SULFURAS.name(), -1, 80);
		Sulfuras sulfuras = new Sulfuras();
		sulfuras.updateQuality(item);
		assertEquals(item.quality, 80);
		assertEquals(item.sellIn, -1);
	}

}
