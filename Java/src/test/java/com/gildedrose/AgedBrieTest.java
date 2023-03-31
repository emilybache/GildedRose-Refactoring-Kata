package com.gildedrose;

import static org.junit.jupiter.api.Assertions.*;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class AgedBrieTest {

	private static Stream<Arguments> getTestItemsWithExpectedQuality() {
		return Stream.of(
				Arguments.of(new Item(GoodsType.AGED_BRIE.getGoodsName(), 2, 2), 3), 
				Arguments.of(new Item(GoodsType.AGED_BRIE.getGoodsName(), 1, 50), 50)
				);
	}
	
	private static Stream<Arguments> getTestItemsWithExpectedSellin() {
		return Stream.of(
				Arguments.of(new Item(GoodsType.AGED_BRIE.getGoodsName(), 2, 2), 1), 
				Arguments.of(new Item(GoodsType.AGED_BRIE.getGoodsName(), 1, 50), 0)
				);
	}
	
	private static Stream<Arguments> getTestExpiredItemsWithExpectedQuality() {
		return Stream.of(
				Arguments.of(new Item(GoodsType.AGED_BRIE.getGoodsName(), -1, 2), 3), 
				Arguments.of(new Item(GoodsType.AGED_BRIE.getGoodsName(), 1, 2), 2)
				);
	}


	@ParameterizedTest(name = "Test updateQuality - {index}")
	@MethodSource("getTestItemsWithExpectedQuality")
	void testUpdateQuality(Item item, int expectedQuality) {
		AgedBrie agedBrie = new AgedBrie();
		agedBrie.updateQuality(item);
		assertEquals(expectedQuality, item.quality);
	}

	@ParameterizedTest(name = "Test updateSellIn - {index}")
	@MethodSource("getTestItemsWithExpectedSellin")
	void testUpdateSellInDaysItem(Item item, int expectedSellin) {
		AgedBrie agedBrie = new AgedBrie();
		agedBrie.updateSellInDays(item);
		assertEquals(expectedSellin, item.sellIn);

	}
	
	@ParameterizedTest(name = "Test updateQuality for expired items - {index}")
	@MethodSource("getTestExpiredItemsWithExpectedQuality")
	void testtestUpdateQualityForExpiredItems(Item item, int expectedQuality) {
		AgedBrie agedBrie = new AgedBrie();
		agedBrie.updateQualityForExpiredItem(item);
		assertEquals(expectedQuality, item.quality);
	}

}
