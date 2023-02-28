package com.gildedrose.model;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class NormalItemTest {

    private static Stream<Arguments> provideInputWhereSellInDayPassesByOneDay() {

        return Stream.of(

            // 1. Normal Item
            // 1.a Before sellIn
            Arguments.of(new NormalItem("Normal Item", 1, 0), "Normal Item", 0, 0),
            Arguments.of(new NormalItem("Normal Item", 1, 10), "Normal Item", 0, 9),

            //1.b After sellIn
            Arguments.of(new NormalItem("Normal Item", 0, 0), "Normal Item", -1, 0),
            Arguments.of(new NormalItem("Normal Item", 0, 10), "Normal Item", -1, 8));


    }

    @ParameterizedTest
    @MethodSource("provideInputWhereSellInDayPassesByOneDay")
    void shouldDegradeQualityForAnItem(NormalItem normalItem, String expectedName, int expectedSellIn, int expectedQuality) {
        normalItem.updateQuality();
        assertEquals(expectedName, normalItem.name);
        assertEquals(expectedSellIn, normalItem.sellIn);
        assertEquals(expectedQuality, normalItem.quality);
    }

}
