package com.gildedrose;

import static org.junit.jupiter.api.Assertions.*;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class ConjuredTest {

	private static Stream<Arguments> getTestItems() {
		return Stream.of(Arguments.of(new Item(GoodsType.CONJURED.getGoodsName(), 2, 1), 0),
				Arguments.of(new Item(GoodsType.CONJURED.getGoodsName(), 10, 5), 3)
				);
	}

	private static Stream<Arguments> getExpiredTestItems() {
		return Stream.of(Arguments.of(new Item(GoodsType.CONJURED.getGoodsName(), -1, 1), 0),
				Arguments.of(new Item(GoodsType.CONJURED.getGoodsName(), -1, 5), 3)
				);
	}

	@ParameterizedTest(name = "Test updateQuality - {index}")
	@MethodSource("getTestItems")
	void testUpdateQuality(Item item, int expectedQuality) {
		Conjured conjured = new Conjured();
		conjured.updateQuality(item);
		assertEquals(expectedQuality, item.quality);

	}

	@ParameterizedTest(name = "Test updateQualityForExpiredItems - {index}")
	@MethodSource("getExpiredTestItems")
	void testUpdateQualityForExpiredItem(Item item, int expectedQuality) {
		Conjured conjured = new Conjured();
		conjured.updateQualityForExpiredItem(item);
		assertEquals(expectedQuality, item.quality);
	}

}
