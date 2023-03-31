package com.gildedrose;

import static org.junit.jupiter.api.Assertions.*;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class GenericTest {

	private static Stream<Arguments> getTestItemsWithExpectedQuality() {
		return Stream.of(
				Arguments.of(new Item("Generic Item", 2, 0), 0), 
				Arguments.of(new Item("Generic Item", 2, 2), 1)
				);
	}
	
	private static Stream<Arguments> getTestItemsWithExpectedSellin() {
		return Stream.of(
				Arguments.of(new Item("Generic Item", 2, 0), 1), 
				Arguments.of(new Item("Generic Item", 2, 2), 1)
				);
	}
	
	private static Stream<Arguments> getTestExpiredItemsWithExpectedQuality() {
		return Stream.of(
				Arguments.of(new Item("Generic Item", -1, 1), 0), 
				Arguments.of(new Item("Generic Item", 0, 2), 2),
				Arguments.of(new Item("Generic Item", -1, -1), -1)
				);
	}



	@ParameterizedTest(name = "Test updateQuality - {index}")
	@MethodSource("getTestItemsWithExpectedQuality")
	void testUpdateQuality(Item item, int expectedQuality) {
		Generic generic = new Generic();
		generic.updateQuality(item);
		assertEquals(expectedQuality, item.quality);
	}

	@ParameterizedTest(name = "Test updateSellIn - {index}")
	@MethodSource("getTestItemsWithExpectedSellin")
	void testUpdateSellInDaysItem(Item item, int expectedSellin) {
		Generic generic = new Generic();
		generic.updateSellInDays(item);
		assertEquals(expectedSellin, item.sellIn);
	}
	
	@ParameterizedTest(name = "Test updateQuality for expired items- {index}")
	@MethodSource("getTestExpiredItemsWithExpectedQuality")
	void testUpdateQualityForExpiredItems(Item item, int expectedQuality) {
		Generic generic = new Generic();
		generic.updateQualityForExpiredItem(item);
		assertEquals(expectedQuality, item.quality);
	}

}
