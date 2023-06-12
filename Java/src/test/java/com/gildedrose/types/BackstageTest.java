package com.gildedrose.types;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.gildedrose.Item;
import com.gildedrose.enums.ProductType;

public class BackstageTest {
	
	@Test
	public void testBackstageUpdateQuality() {
		Item item = new Item(ProductType.BACKSTAGE_PASSES.name(), 2, 3);
		Backstage backstage = new Backstage();
		backstage.updateQuality(item);
		assertEquals(item.quality, 4);
		assertEquals(item.sellIn, 1);
	}

}
