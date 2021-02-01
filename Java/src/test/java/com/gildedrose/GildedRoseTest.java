package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

	@Test
	void foo() {
		Item[] items = new Item[] { new Item("foo", 0, 0) };
		GildedRose app = new GildedRose(items);
		app.updateQuality();
		assertEquals("foo", app.items[0].name);
		assertEquals(0, app.items[0].quality);
		assertEquals(-1, app.items[0].sellIn);

	}

	@Test
	public void shouldLowerBothValues() {
		Item[] items = new Item[] { new Item("foobar", 1, 1) };
		GildedRose app = new GildedRose(items);
		app.updateQuality();
		assertEquals(0, app.items[0].quality);
		assertEquals(0, app.items[0].sellIn);
	}

	@Test
	public void shouldDowngradeTwiceAsFastAfterSellDate() {
		Item[] items = new Item[] { new Item("foobar", 1, 5) };
		GildedRose app = new GildedRose(items);

		// day 1, drop by 1
		app.updateQuality();
		assertEquals(4, app.items[0].quality);
		assertEquals(0, app.items[0].sellIn);

		// day 2, drop by 2
		app.updateQuality();
		assertEquals(2, app.items[0].quality);
		assertEquals(-1, app.items[0].sellIn);
	}

	@Test
	public void shouldNeverHaveANegativeQuality() {
		Item[] items = new Item[] { new Item("foobar", 0, 0) };
		GildedRose app = new GildedRose(items);

		// day 1, drop by 1
		app.updateQuality();
		assertEquals(0, app.items[0].quality);

		// day 2, drop by 1 => quality is still 0
		app.updateQuality();
		assertEquals(0, app.items[0].quality);
	}

	@Test
	public void shouldSeeAgedBrieIncreasedQualityDayByDay() {
		Item[] items = new Item[] { new Item("Aged Brie", 0, 0) };
		GildedRose app = new GildedRose(items);

		// day 1, add 2
		app.updateQuality();
		assertEquals(2, app.items[0].quality);

		// day 2, add 2
		app.updateQuality();
		assertEquals(4, app.items[0].quality);
	}

	@Test
	public void shouldNeverHaveQualityHigherThan50() {
		Item[] items = new Item[] { new Item("Aged Brie", 0, 49) };
		GildedRose app = new GildedRose(items);

		// day 1, add 2
		app.updateQuality();
		assertEquals(50, app.items[0].quality);

		// day 2, add 2
		app.updateQuality();
		assertEquals(50, app.items[0].quality);
	}

	@Test
	public void shouldSulfrasNotChangeQty() {
		Item[] items = new Item[] { new Item("Sulfuras, Hand of Ragnaros", 0, 10) };
		GildedRose app = new GildedRose(items);

		// day 1, don't impact quality
		app.updateQuality();
		assertEquals(10, app.items[0].quality);
		assertEquals(0, app.items[0].sellIn);

	}

	@Test
	public void checkForBackStagePasses() {
		Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 5, 10) };
		GildedRose app = new GildedRose(items);

		// day 1, increase by 3 the quality
		app.updateQuality();
		assertEquals(13, app.items[0].quality);
		assertEquals(4, app.items[0].sellIn);

		// day 1, increase by 3 the quality
		app.updateQuality();
		assertEquals(16, app.items[0].quality);
		assertEquals(3, app.items[0].sellIn);
	}

	@Test
	public void shouldIncreaseQualityBasedOnSellIn() {
		Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 10, 10),
				new Item("Backstage passes to a TAFKAL80ETC concert", 5, 10) };
		GildedRose app = new GildedRose(items);

		app.updateQuality();
		assertEquals(12, app.items[0].quality);
		assertEquals(9, app.items[0].sellIn);

		assertEquals(13, app.items[1].quality);
		assertEquals(4, app.items[1].sellIn);
	}

	public void shouldSulfurasNeverAltersForSulfuras() {
		Item[] items = new Item[] { new Item("Sulfuras, Hand of Ragnaros", 10, 80) };

		GildedRose app = new GildedRose(items);

		app.updateQuality();
		assertEquals(80, app.items[0].quality);
	}

	@Test
	public void shouldHaveQualityDroppingToZeroAfterConcert() {
		Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 0, 10) };

		GildedRose app = new GildedRose(items);

		app.updateQuality();
		assertEquals(0, app.items[0].quality);
		assertEquals(-1, app.items[0].sellIn);
	}

	@Test
	public void shouldDegradeConjureItemsTwiceAsNormal() {
		Item[] items = new Item[] { new Item("Conjured Mana Cake", 10, 10), new Item("Conjured Mana Cake", 0, 10) };

		GildedRose app = new GildedRose(items);

		app.updateQuality();
		assertEquals(8, app.items[0].quality);
		assertEquals(6, app.items[1].quality);
	}
}
