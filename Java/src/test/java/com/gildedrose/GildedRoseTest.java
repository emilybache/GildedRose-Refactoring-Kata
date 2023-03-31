package com.gildedrose;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

class GildedRoseTest {
	
	private static Stream<Arguments> getTestItemsForSellInDayCheck() {
		return Stream.of(
				Arguments.of(new Item[] { new Item(GoodsType.AGED_BRIE.getGoodsName(), 1, 1) }, 0),
				Arguments.of(new Item[] { new Item(GoodsType.SULFURAS.getGoodsName(), 1, 1) }, 1),
				Arguments.of(new Item[] { new Item(GoodsType.CONJURED.getGoodsName(), 1, 1) }, 0)

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
		return Stream.of(
				Arguments.of(new Item[] { new Item(GoodsType.AGED_BRIE.getGoodsName(), 1, 1) }, 2),
				Arguments.of(new Item[] { new Item(GoodsType.AGED_BRIE.getGoodsName(), 1, 50) }, 50),

				Arguments.of(new Item[] { new Item(GoodsType.AGED_BRIE.getGoodsName(), -1, 1) }, 3),
				Arguments.of(new Item[] { new Item(GoodsType.AGED_BRIE.getGoodsName(), -1, 50) }, 50),

				Arguments.of(new Item[] { new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 5, 10) }, 13),
				Arguments.of(new Item[] { new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 5, 50) }, 50),
				Arguments.of(new Item[] { new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 10, 10) }, 12),
				Arguments.of(new Item[] { new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 10, 50) }, 50),
				Arguments.of(new Item[] { new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 12, 10) }, 11),
				Arguments.of(new Item[] { new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 12, 50) }, 50),

				Arguments.of(new Item[] { new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), -1, 10) }, 0),

				Arguments.of(new Item[] { new Item("Coffee Day", 1, 1) }, 0),
				Arguments.of(new Item[] { new Item("Coffee Day", -1, 1) }, 0),
				Arguments.of(new Item[] { new Item("Coffee Day", -1, 2) }, 0),
				
				Arguments.of(new Item[] { new Item(GoodsType.SULFURAS.getGoodsName(), -1, 10) }, 10),
				Arguments.of(new Item[] { new Item(GoodsType.SULFURAS.getGoodsName(), 1, 10) }, 10),
				
				Arguments.of(new Item[] { new Item(GoodsType.CONJURED.getGoodsName(), -1, 10) }, 6),
				Arguments.of(new Item[] { new Item(GoodsType.CONJURED.getGoodsName(), 1, 10) }, 8),
				Arguments.of(new Item[] { new Item(GoodsType.CONJURED.getGoodsName(), -1, 2) }, 0),
				Arguments.of(new Item[] { new Item(GoodsType.CONJURED.getGoodsName(), 1, 2) }, 0)

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
