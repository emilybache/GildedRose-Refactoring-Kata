package com.gildedrose.config;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.gildedrose.enums.ProductType;
import com.gildedrose.generic.ItemType;
import com.gildedrose.types.AgedBrie;
import com.gildedrose.types.Backstage;
import com.gildedrose.types.Others;

public class ProductFactoryTest {
	
	@Test
	public void testGetItems() {
		ItemType agedBreiItemType = ProductFactory.get(ProductType.AGED_BRIE);
		assertTrue(agedBreiItemType instanceof AgedBrie);
		
		ItemType backstageItemType = ProductFactory.get(ProductType.BACKSTAGE_PASSES);
		assertTrue(backstageItemType instanceof Backstage);
		
		ItemType otherItemType = ProductFactory.get(null);
		assertTrue(otherItemType instanceof Others);
	}

}
