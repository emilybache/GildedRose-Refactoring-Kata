package com.gildedrose;

import static org.junit.jupiter.api.Assertions.*;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class SulfurasTest {

	private static Stream<Arguments> getTestItemsWithExpectedQuality() {
		return Stream.of(
				Arguments.of(new Item(GoodsType.SULFURAS.getGoodsName(), 2, 2), 2),
				Arguments.of(new Item(GoodsType.SULFURAS.getGoodsName(), 1, 50), 50));
	}

	private static Stream<Arguments> getTestItemsWithExpectedSellin() {
		return Stream.of(
				Arguments.of(new Item(GoodsType.SULFURAS.getGoodsName(), 2, 2), 2),
				Arguments.of(new Item(GoodsType.SULFURAS.getGoodsName(), 1, 50), 1));
	}

	@ParameterizedTest(name = "Test updateQuality - {index}")
	@MethodSource("getTestItemsWithExpectedQuality")
	void testUpdateQuality(Item item, int expectedQuality) {
		Sulfuras sulfurus = new Sulfuras();
		sulfurus.updateQuality(item);
		assertEquals(expectedQuality, item.quality);
	}

	@ParameterizedTest(name = "Test updateSellin - {index}")
	@MethodSource("getTestItemsWithExpectedSellin")
	void testUpdateSellInDays(Item item, int expectedSellin) {
		Sulfuras sulfurus = new Sulfuras();
		sulfurus.updateSellInDays(item);
		assertEquals(expectedSellin, item.sellIn);
	}

}
