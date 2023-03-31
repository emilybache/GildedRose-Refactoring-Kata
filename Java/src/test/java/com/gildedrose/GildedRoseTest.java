package com.gildedrose;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

class GildedRoseTest {
	
	private static Stream<Arguments> getTestItemsForSellInDayCheck() {
		return Stream.of(
				Arguments.of(new Item[] { new Item("Aged Brie", 1, 1) }, 0),
				Arguments.of(new Item[] { new Item("Sulfuras, Hand of Ragnaros", 1, 1) }, 1)

		);
	}

	@ParameterizedTest(name = "Test sellin days for items - {index}")
	@MethodSource("getTestItemsForSellInDayCheck")
	void testUpdateSellInDays(Item[] items, int expectedSellinDays) {
		GildedRose app = new GildedRose(items);
		app.updateQuality();
		assertEquals(expectedSellinDays, items[0].sellIn);
	}
	
	private static Stream<Arguments> getTestItemsForQualityCheck() {
		return Stream.of(Arguments.of(new Item[] { new Item("Aged Brie", 1, 1) }, 2),
				Arguments.of(new Item[] { new Item("Aged Brie", 1, 50) }, 50),

				Arguments.of(new Item[] { new Item("Aged Brie", -1, 1) }, 3),
				Arguments.of(new Item[] { new Item("Aged Brie", -1, 50) }, 50),

				Arguments.of(new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 5, 10) }, 13),
				Arguments.of(new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 5, 50) }, 50),
				Arguments.of(new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 10, 10) }, 12),
				Arguments.of(new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 10, 50) }, 50),
				Arguments.of(new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 12, 10) }, 11),
				Arguments.of(new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", 12, 50) }, 50),

				Arguments.of(new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert", -1, 10) }, 0)

		);
	}

	@ParameterizedTest(name = "Test quality for non expired and expired items - {index}")
	@MethodSource("getTestItemsForQualityCheck")
	void testUpdateQuality(Item[] items, int expectedQuality) {
		GildedRose app = new GildedRose(items);
		app.updateQuality();
		assertEquals(expectedQuality, items[0].quality);
	}

}
