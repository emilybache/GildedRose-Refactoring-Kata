package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {
	@Test
	void testUpdateSellInDaysForAgedBrie() {
		Item[] items = new Item[] { new Item("Aged Brie", 1, 1) };
		GildedRose app = new GildedRose(items);
		app.updateQuality();
		assertEquals(0, items[0].sellIn);
	}

	@Test
	void testUpdateSellInDaysForSulfuras() {
		Item[] items = new Item[] { new Item("Sulfuras, Hand of Ragnaros", 1, 1) };
		GildedRose app = new GildedRose(items);
		app.updateQuality();
		assertEquals(1, items[0].sellIn);
	}

}
