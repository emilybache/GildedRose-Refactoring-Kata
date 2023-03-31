package com.gildedrose;

import static org.junit.jupiter.api.Assertions.*;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BackStagePassesTest {
	private static Stream<Arguments> getTestItemsWithExpectedQuality() {
		return Stream.of(
				Arguments.of(new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 2, 2), 5),
				Arguments.of(new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 10, 45), 47),
				Arguments.of(new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 0, 49), 50),
				Arguments.of(new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 12, 46), 47),
				Arguments.of(new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 3, 50), 50));
	}
	
	private static Stream<Arguments> getTestItemsWithExpectedSellin() {
		return Stream.of(
				Arguments.of(new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 2, 2), 1), 
				Arguments.of(new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 1, 50), 0)
				);
	}
	
	private static Stream<Arguments> getTestExpiredItemsWithExpectedQuality() {
		return Stream.of(
				Arguments.of(new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), -1, 2), 0),
				Arguments.of(new Item(GoodsType.BACK_STAGE_PASSES.getGoodsName(), 1, 2), 2));
	}



	@ParameterizedTest(name = "Test updateQuality - {index}")
	@MethodSource("getTestItemsWithExpectedQuality")
	void testUpdateQuality(Item item, int expectedQuality) {
		BackStagePasses backStagePasses = new BackStagePasses();
		backStagePasses.updateQuality(item);
		assertEquals(expectedQuality, item.quality);
	}

	@ParameterizedTest(name = "Test updateSellIn - {index}")
	@MethodSource("getTestItemsWithExpectedSellin")
	void testUpdateSellInDaysItem(Item item, int expectedSellin) {
		BackStagePasses backStagePasses = new BackStagePasses();
		backStagePasses.updateSellInDays(item);
		assertEquals(expectedSellin, item.sellIn);
	}
	
	@ParameterizedTest(name = "Test updateQuality for expired items - {index}")
	@MethodSource("getTestExpiredItemsWithExpectedQuality")
	void testUpdateQualityForExpiredItems(Item item, int expectedQuality) {
		BackStagePasses backStagePasses = new BackStagePasses();
		backStagePasses.updateQualityForExpiredItem(item);
		assertEquals(expectedQuality, item.quality);
	}

}
